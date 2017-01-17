module Informer.Monitor
    ( Monitor
    , Config(..)
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

import Informer.Report
import Informer.Systemd
import Informer.Util

data Monitor = Monitor
    { monitorStateVar :: TVar StateTable
    , monitorDBusClient :: DBus.Client
    , monitorSubscriptions :: TVar [DBus.SignalHandler]
    , monitorKernelTimestamp :: LocalTime
    }

data UnitState = UnitState
    { unitLastState :: ActiveState
    , unitLastSeen :: LocalTime
    , unitLastGood :: Maybe LocalTime
    }

type StateTable = Map String UnitState

data Config = Config

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
    let monitor = Monitor stateVar dbusClient subscriptionsVar kernelTimestamp

    -- Subscribe to JobRemoved events on the main systemd instance
    jobSubscription <- registerJobHandler dbusClient (handleJobRemoved monitor)
    atomically $ modifyTVar' subscriptionsVar (jobSubscription:)

    -- At this point, if some units were already failed we will handle them
    forkIO $ handleInitialNotification monitor initialUnits

    return monitor

stopMonitor :: Monitor -> IO ()
stopMonitor monitor = do return ()

handleInitialNotification :: Monitor -> [Unit] -> IO ()
handleInitialNotification monitor units = do
    let kernelTimestamp = monitorKernelTimestamp monitor
        failedUnits = filter (stateIsFailure . unitActive) units
        unitsWithTimestamp = zip failedUnits (repeat kernelTimestamp)
        dbusClient = monitorDBusClient monitor

    when (failedUnits /= []) $ handleUnitsFailed dbusClient unitsWithTimestamp

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
    printerr $ printf "%s entered %s state (was %s)\n"
        name (show $ currentState) (maybe "unknown" show prevState)

    -- Send notification if newly failed
    when newlyFailed $ handleUnitsFailed dbusClient [(unit, lastGoodTimestamp)]

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

handleUnitsFailed dbusClient unitsWithTimestamps =
    collectData dbusClient unitsWithTimestamps >>= dispatchNotification

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

dispatchNotification :: ReportData -> IO ()
dispatchNotification = printerr . formatReport

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