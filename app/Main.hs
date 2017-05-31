module Main (main) where

import Control.Concurrent
import Control.Concurrent.MVar
import qualified DBus.Client as DBus
import System.Posix.Signals

import Informer.Monitor
import Informer.Util

main = do
    dbusClient <- DBus.connectSystem

    let config = defaultConfig { configDebug = True }

    monitor <- startMonitor dbusClient config

    mainLoop <- newEmptyMVar
    installHandler sigINT (Catch (signalHandler mainLoop)) Nothing

    printerr "Listening to Systemd..."

    -- Block main thread forever or until we catch a signal
    takeMVar mainLoop

-- On signal, release the semaphore to exit the main loop
signalHandler mainLoop = do
    printerr "Caught deadly signal, exiting"
    putMVar mainLoop ()
