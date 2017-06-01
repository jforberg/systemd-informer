{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
import Data.Foldable
import Data.Semigroup
import qualified DBus.Client as DBus
import Options.Applicative
import System.Exit
import System.IO
import System.Posix.Signals

import Informer.Monitor
import Informer.Util

main :: IO ()
main = do
    args <- execParser argParser

    dbusClient <- DBus.connectSystem

    mainLoop <- newEmptyMVar

    for_ [ sigINT, sigQUIT, sigTERM ] $ \s ->
        installHandler s (CatchInfo (signalHandler mainLoop)) Nothing

    monitor <- startMonitor dbusClient args

    printerr "Listening to Systemd..."

    -- Block main thread forever or until we catch a signal
    takeMVar mainLoop

    -- Got fatal signal
    stopMonitor monitor
    exitFailure

argDef = Config
    <$> switch (long "debug" <> short 'd' <> help "debug mode")
    <*> switch (long "stdout" <> short 's' <> help "print to stdout; don't send emails")

argParser = info (argDef <**> helper) fullDesc

-- On signal, release the semaphore to exit the main loop
signalHandler mainLoop siginfo = do
    let sig = siginfoSignal siginfo
    hPutStrLn stderr $ "Caught deadly signal " ++ show sig ++ ", exiting"
    putMVar mainLoop ()
