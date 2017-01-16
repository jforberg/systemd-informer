module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Time
import DBus.Client
import Network.HostName
import System.IO
import System.Posix.Signals
import System.Process
import Text.Printf

import Informer.Report
import Informer.Systemd

data UnitState = UnitState
    { unitLastState :: ActiveState
    , unitLastSeen :: LocalTime
    , unitLastGood :: Maybe LocalTime
    }

type StateTable = Map.Map String UnitState

main = do
    client <- connectSystem
    initialUnitList <- listUnits client
    currentTimestamp <- getCurrentLocalTime

    let initialState = buildInitialStateTable initialUnitList currentTimestamp

    tableVar <- atomically $ newTVar initialState
    registerJobHandler client (handleJobRemoved client tableVar)

    mainLoop <- newEmptyMVar
    installHandler sigINT (Catch (signalHandler mainLoop)) Nothing

    hPutStrLn stderr "Listening to systemd..."

    -- Maybe some units are failed already at startup?
    kernelTimestamp <- getKernelTimestamp client
    handleInitialNotification client initialUnitList currentTimestamp

    -- block main thread until we get SIGINT.
    takeMVar mainLoop

signalHandler mainLoop = putMVar mainLoop ()

handleInitialNotification :: Client -> [Unit] -> LocalTime -> IO ()
handleInitialNotification client initialUnitList kernelTimestamp = do
    let failedUnits = filter (stateIsFailure . unitActive) initialUnitList
        unitsWithTimestamp = zip failedUnits (repeat kernelTimestamp)
    when (failedUnits /= []) $ dispatchNotification client unitsWithTimestamp

handleJobRemoved :: Client -> TVar StateTable -> JobInfo -> IO ()
handleJobRemoved client tableVar jobInfo = do
    -- Sleep for a short time to make sure Systemd is completely don
--    threadDelay 1000

    -- Read info for the affected unit
    unit <- getUnit client (jobInfoUnitName jobInfo)
    currentTimestamp <- getCurrentLocalTime

    -- Update the state table with new information and return Maybe previous state.
    prevUnitState <- atomically $ do
        stateTable <- readTVar tableVar
        writeTVar tableVar $ updateStateTable currentTimestamp unit stateTable
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
    hPrintf stderr "%s entered %s state (was %s)\n"
        name (show $ currentState) (maybe "unknown" show prevState)

    -- Send notification if newly failed
    when newlyFailed $ dispatchNotification client [(unit, lastGoodTimestamp)]

dispatchNotification :: Client -> [(Unit, LocalTime)] -> IO ()
dispatchNotification client unitList = do
    reportData <- collectData client unitList
    hPutStrLn stderr $ formatReport reportData

buildInitialStateTable :: [Unit] -> LocalTime -> StateTable
buildInitialStateTable units currentTime = foldr (updateStateTable currentTime) Map.empty units

updateStateTable :: LocalTime -> Unit -> StateTable -> StateTable
updateStateTable timestamp u table = Map.alter f name table
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

stateIsFailure = (== UnitFailed)

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = getZonedTime >>= return . zonedTimeToLocalTime

collectData :: Client -> [(Unit, LocalTime)] -> IO ReportData
collectData client unitsWithTimestamps = do
    hostname <- getHostName
    kernelTimestamp <- getKernelTimestamp client
    journals <- collectJournals unitsWithTimestamps

    return ReportData { reportFailedUnits = map fst unitsWithTimestamps
                      , reportHostname = hostname
                      , reportKernelTimestamp = kernelTimestamp
                      , reportJournals = journals
                      }

collectJournals :: [(Unit, LocalTime)] -> IO (Map.Map String String)
collectJournals = foldM addJournalToMap Map.empty

addJournalToMap :: Map.Map String String -> (Unit, LocalTime) -> IO (Map.Map String String)
addJournalToMap map (unit, sinceTimestamp) = do
    let name = unitName unit
    journal <- journalForUnit name sinceTimestamp
    return $ Map.insert name journal map

journalForUnit :: String -> LocalTime -> IO String
journalForUnit unitName sinceTimestamp = readProcess "/usr/bin/journalctl" args ""
    where args = [ "-u", unitName, "-S", timestampString ]
          timestampString = formatTime defaultTimeLocale "%F %T" sinceTimestamp

