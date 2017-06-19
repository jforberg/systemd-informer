{-# LANGUAGE OverloadedStrings #-}

module Informer.Monitor
( Monitor
, Config(..)
, startMonitor
, stopMonitor
)
where

import Control.Concurrent.STM
import Control.Monad
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified DBus.Client as DBus
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Format
import qualified Data.Text.IO as TIO
import Data.Time
import Network.HostName

import Informer.Journal
import Informer.Notification
import Informer.Report
import Informer.Systemd
import Informer.Util

data Monitor = Monitor
    { monStateVar :: TVar StateTable
    , monDbusClient :: DBus.Client
    , monSubsVar :: TVar [DBus.SignalHandler]
    , monKernelTimestamp :: LocalTime
    , monConfig :: Config
    }

data UnitState = UnitState
    { unitLastState :: ActiveState
    , unitLastSeen :: LocalTime
    , unitLastGood :: Maybe LocalTime
    }
    deriving Show

type StateTable = Map UnitName UnitState

data Config = Config
    { configDebug :: Bool
    , configStdout :: Bool
    }
    deriving Show

data NotificationTarget = EmailTarget | StdoutTarget

startMonitor :: DBus.Client -> Config -> IO Monitor
startMonitor dbusClient config = do
    -- Check which units are active/failed on startup and build initial state
    initialUnits <- listUnits dbusClient
    now <- currentTime
    kernelTimestamp <- getKernelTimestamp dbusClient

    let initialState = buildInitialState initialUnits now
    stateVar <- atomically $ newTVar initialState
    subsVar <- atomically $ newTVar []

    -- Construct monitor context
    let mon = Monitor { monStateVar = stateVar
                      , monDbusClient = dbusClient
                      , monSubsVar = subsVar
                      , monKernelTimestamp = kernelTimestamp
                      , monConfig = config
                      }

    -- Subscribe to JobRemoved events on the main systemd instance
    jobSubscription <- registerJobHandler dbusClient (handleJobRemoved mon)
    atomically $ modifyTVar' subsVar (jobSubscription:)

    -- At this point, if some units were already failed we will handle them
    handleInitialNotification mon initialUnits

    return mon

stopMonitor :: Monitor -> IO ()
stopMonitor mon = do
    let var = monSubsVar mon
        client = monDbusClient mon

    subs <- atomically $ readTVar var

    for_ subs $ removeJobHandler client

    void $ atomically $ swapTVar var []
    return ()

handleInitialNotification :: Monitor -> [Unit] -> IO ()
handleInitialNotification mon units = do
    let kernelTimestamp = monKernelTimestamp mon
        failedUnits = filter (stateIsFailure . unitActive) units
        unitsWithTimestamp = zip failedUnits (repeat kernelTimestamp)
        debug = configDebug . monConfig $ mon

    when (failedUnits /= []) $ do
        when debug $
            printerr $ formatStrict "{} units were failed an startup" $
                Only (showText . length $ failedUnits)
        handleUnitsFailed mon unitsWithTimestamp

handleJobRemoved :: Monitor -> JobInfo -> IO ()
handleJobRemoved mon jobInfo = do
    let dbus = monDbusClient mon
        stateVar = monStateVar mon
        debug = configDebug . monConfig $ mon
        name = jobInfoUnitName jobInfo

    -- Read info for the affected unit
    unit <- getUnit dbus name
    currentTimestamp <- currentTime

    -- Update the state table with new information and return Maybe previous state.
    prevUnitState <- atomically $ do
        stateTable <- readTVar stateVar
        writeTVar stateVar $ updateState currentTimestamp unit stateTable
        return $ unitName unit `Map.lookup` stateTable

    let prevState = fmap unitLastState prevUnitState
        prevStateFailed = maybe False stateIsFailure prevState
        lastGoodTimestamp = case prevUnitState of
            Just state -> currentTimestamp `fromMaybe` unitLastGood state
            Nothing -> currentTimestamp
        currentState = unitActive unit
        currentStateFailed = stateIsFailure currentState
        newlyFailed = currentStateFailed && not prevStateFailed

    -- Debug info
    when debug $
        printerr $ formatStrict "{} entered {} state (was {})"
            [ T.pack . unUnitName $ name
            , T.pack . show $ currentState
            , maybe "unknown" (T.pack . show) prevState
            ]

    -- Send notification if newly failed
    when newlyFailed $ handleUnitsFailed mon [(unit, lastGoodTimestamp)]

buildInitialState :: [Unit] -> LocalTime -> StateTable
buildInitialState us t =
    foldr (updateState t) Map.empty us

updateState :: LocalTime -> Unit -> StateTable -> StateTable
updateState t u = Map.alter f name
    where f Nothing = Just UnitState { unitLastState = currentState
                                     , unitLastSeen = t
                                     , unitLastGood = if failed then Nothing else Just t
                                     }
          f (Just old) | failed    = Just old { unitLastState = currentState
                                              , unitLastSeen = t
                                              }
                       | otherwise = Just old { unitLastState = currentState
                                              , unitLastSeen = t
                                              , unitLastGood = Just t
                                              }
          name = unitName u
          currentState = unitActive u
          failed = stateIsFailure currentState

handleUnitsFailed mon unitsWithTimestamps = do
    reportData <- collectData (monDbusClient mon) unitsWithTimestamps

    if configStdout . monConfig $ mon
        then dispatchNotification reportData StdoutTarget
        else dispatchNotification reportData EmailTarget

collectData :: DBus.Client -> [(Unit, LocalTime)] -> IO ReportData
collectData client unitsWithTimestamps = do
    hostname <- getHostName
    kernelTimestamp <- getKernelTimestamp client
    journals <- journalsForUnits unitsWithTimestamps

    return ReportData { rdFailedUnits = map fst unitsWithTimestamps
                      , rdHostname = T.pack hostname
                      , rdKernelTimestamp = kernelTimestamp
                      , rdJournals = journals
                      }

dispatchNotification :: ReportData -> NotificationTarget -> IO ()
dispatchNotification reportData target = do
    let report = formatReport reportData
    case target of
        EmailTarget -> sendNotification report
        StdoutTarget -> TIO.putStrLn report

stateIsFailure = (== UnitFailed)

currentTime :: IO LocalTime
currentTime = zonedTimeToLocalTime <$> getZonedTime
