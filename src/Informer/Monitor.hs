module Informer.Monitor
    ( Monitor
    , Config(..)
    , defaultConfig
    , startMonitor
    , stopMonitor
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified DBus.Client as DBus
import Data.Time
import Network.HostName
import System.Process
import Text.Printf

import Informer.Notifications
import Informer.Report
import Informer.Systemd
import Informer.Util

data Monitor = Monitor
    { monitorStateVar :: TVar StateTable
    , monitorDBusClient :: DBus.Client
    , monitorSubscriptionsVar :: TVar [DBus.SignalHandler]
    , monitorKernelTimestamp :: LocalTime
    , monitorConfig :: Config
    }

data UnitState = UnitState
    { unitLastState :: ActiveState
    , unitLastSeen :: LocalTime
    , unitLastGood :: Maybe LocalTime
    }

type StateTable = Map String UnitState

data Config = Config
    { configDebug :: Bool
    , configStdout :: Bool
    }

data NotificationTarget = EmailTarget | StdoutTarget

defaultConfig :: Config
defaultConfig = Config
    { configDebug = False
    , configStdout = False
    }

startMonitor :: DBus.Client -> Config -> IO Monitor
startMonitor dbusClient config = do
    -- Check which units are active/failed on startup and build initial state
    initialUnits <- listUnits dbusClient
    currentTime <- currentTime
    kernelTimestamp <- getKernelTimestamp dbusClient

    let initialState = buildInitialState initialUnits currentTime
    stateVar <- atomically $ newTVar initialState
    subscriptionsVar <- atomically $ newTVar []

    -- Construct monitor context
    let monitor = Monitor { monitorStateVar = stateVar
                          , monitorDBusClient = dbusClient
                          , monitorSubscriptionsVar = subscriptionsVar
                          , monitorKernelTimestamp = kernelTimestamp
                          , monitorConfig = config
                          }

    -- Subscribe to JobRemoved events on the main systemd instance
    jobSubscription <- registerJobHandler dbusClient (handleJobRemoved monitor)
    atomically $ modifyTVar' subscriptionsVar (jobSubscription:)

    -- At this point, if some units were already failed we will handle them
    handleInitialNotification monitor initialUnits

    return monitor

stopMonitor :: Monitor -> IO ()
stopMonitor monitor = do return ()

handleInitialNotification :: Monitor -> [Unit] -> IO ()
handleInitialNotification monitor units = do
    let kernelTimestamp = monitorKernelTimestamp monitor
        failedUnits = filter (stateIsFailure . unitActive) units
        unitsWithTimestamp = zip failedUnits (repeat kernelTimestamp)
        dbusClient = monitorDBusClient monitor

    when (failedUnits /= []) $ do
        when (configDebug . monitorConfig $ monitor) $ do
            printerr $ printf "%d units were failed on startup" (length failedUnits)
        handleUnitsFailed monitor unitsWithTimestamp

handleJobRemoved :: Monitor -> JobInfo -> IO ()
handleJobRemoved monitor jobInfo = do
    let dbusClient = monitorDBusClient monitor
        stateVar = monitorStateVar monitor

    -- Read info for the affected unit
    unit <- getUnit dbusClient (jobInfoUnitName jobInfo)
    currentTimestamp <- currentTime

    -- Update the state table with new information and return Maybe previous state.
    prevUnitState <- atomically $ do
        stateTable <- readTVar stateVar
        writeTVar stateVar $ updateState currentTimestamp unit stateTable
        return $ unitName unit `Map.lookup` stateTable

    let name = unitName unit
        prevState = fmap unitLastState prevUnitState
        prevStateFailed = maybe False stateIsFailure prevState
        lastGoodTimestamp = case prevUnitState of
            Just state -> currentTimestamp `fromMaybe` unitLastGood state
            Nothing -> currentTimestamp
        currentState = unitActive unit
        currentStateFailed = stateIsFailure currentState
        newlyFailed = currentStateFailed && (not prevStateFailed)

    -- Debug info
    when (configDebug . monitorConfig $ monitor) $ do
        printerr $ printf "%s entered %s state (was %s)"
            name (show $ currentState) (maybe "unknown" show prevState)

    -- Send notification if newly failed
    when newlyFailed $ handleUnitsFailed monitor [(unit, lastGoodTimestamp)]

buildInitialState :: [Unit] -> LocalTime -> StateTable
buildInitialState units currentTime =
    foldr (updateState currentTime) Map.empty units

updateState :: LocalTime -> Unit -> StateTable -> StateTable
updateState timestamp u table = Map.alter f name table
    where f Nothing = Just UnitState { unitLastState = currentState
                                     , unitLastSeen = timestamp
                                     , unitLastGood = if failed then Nothing else Just timestamp
                                     }
          f (Just old) | failed    = Just old { unitLastState = currentState
                                              , unitLastSeen = timestamp
                                              }
                       | otherwise = Just old { unitLastState = currentState
                                              , unitLastSeen = timestamp
                                              , unitLastGood = Just timestamp
                                              }
          name = unitName u
          currentState = unitActive u
          failed = stateIsFailure currentState

handleUnitsFailed monitor unitsWithTimestamps = do
    reportData <- collectData (monitorDBusClient monitor) unitsWithTimestamps

    if configStdout . monitorConfig $ monitor
        then dispatchNotification reportData StdoutTarget
        else dispatchNotification reportData EmailTarget

collectData :: DBus.Client -> [(Unit, LocalTime)] -> IO ReportData
collectData client unitsWithTimestamps = do
    hostname <- getHostName
    kernelTimestamp <- getKernelTimestamp client
    journals <- collectJournals unitsWithTimestamps

    return ReportData { reportFailedUnits = map fst unitsWithTimestamps
                      , reportHostname = hostname
                      , reportKernelTimestamp = kernelTimestamp
                      , reportJournals = journals
                      }

collectJournals :: [(Unit, LocalTime)] -> IO (Map String String)
collectJournals = foldM addJournalToMap Map.empty

dispatchNotification :: ReportData -> NotificationTarget -> IO ()
dispatchNotification reportData target = do
    let report = formatReport reportData
    case target of
        EmailTarget -> sendNotification report
        StdoutTarget -> putStrLn report

addJournalToMap :: Map String String -> (Unit, LocalTime) -> IO (Map String String)
addJournalToMap map (unit, sinceTimestamp) = do
    let name = unitName unit
    journal <- journalForUnit name sinceTimestamp
    return $ Map.insert name journal map

journalForUnit :: String -> LocalTime -> IO String
journalForUnit unitName sinceTimestamp = readProcess "/usr/bin/journalctl" args ""
    where args = [ "-u", unitName, "-S", timestampString ]
          timestampString = formatTime defaultTimeLocale "%F %T" sinceTimestamp

stateIsFailure = (== UnitFailed)

currentTime :: IO LocalTime
currentTime = getZonedTime >>= return . zonedTimeToLocalTime
