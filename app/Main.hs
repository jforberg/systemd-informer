module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.List
import Data.Word
import qualified Data.Map.Strict as Map
import DBus.Client
import Network.HostName
import System.IO
import System.Posix.Signals
import System.Process

import Informer.Report
import Informer.Systemd

main = do
    client <- connectSystem

    registerJobHandler client (handleJobRemoved client)

    mainLoop <- newEmptyMVar

    installHandler sigINT (Catch (signalHandler mainLoop)) Nothing

    printUnits client

    hPutStrLn stderr "Listening to systemd..."

    -- block main thread until we get SIGINT.
    takeMVar mainLoop

signalHandler mainLoop = putMVar mainLoop ()

handleJobRemoved client jobInfo = do
    threadDelay 1000
    hPutStrLn stderr "Caught JobRemoved."

    print jobInfo

    getUnit client (jobInfoUnitName jobInfo)

    when (jobInfoResult jobInfo == "failed") $ do
        reportData <- collectData client
        hPutStrLn stderr $ formatReport reportData

printUnits client = do
    units <- listUnits client

    let failed = filter (\u -> unitActive u == UnitFailed) units
    let sorted = sortBy (\u1 u2 -> compare (unitName u1) (unitName u2)) failed

    forM_ sorted $ \u ->
        hPutStrLn stderr $ intercalate " " [unitName u, show $ unitLoaded u, show $ unitActive u]

collectData client = do
    units <- listUnits client
    let failedUnits = filter (\u -> unitActive u == UnitFailed) units

    hostname <- getHostName

    kernelTimestamp <- kernelTimestamp client

    journals <- collectJournals failedUnits kernelTimestamp

    return ReportData { reportFailedUnits = failedUnits
                      , reportHostname = hostname
                      , reportKernelTimestamp = kernelTimestamp
                      , reportJournals = journals
                      }

collectJournals :: [Unit] -> Word64 -> IO (Map.Map String String)
collectJournals units since = foldM (addJournalToMap since) Map.empty units

addJournalToMap since map unit = do
    let name = unitName unit
    journal <- journalForUnit name since
    return $ Map.insert name journal map

journalForUnit :: String -> Word64 -> IO String
journalForUnit unitName since = print args >> readProcess "/usr/bin/journalctl" args ""
    where args = [ "-u", unitName, "-S", "@"++(show since) ]
