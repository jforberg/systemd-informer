{-# LANGUAGE OverloadedStrings #-}

module Informer.Journal
( journalsForUnits
, journalForUnit
)
where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T

import Informer.Systemd
import Informer.Util

journalsForUnits :: [(Unit, LocalTime)] -> IO (Map UnitName Text)
journalsForUnits = foldM add M.empty
    where add m (unit, sinceTimestamp) = do
              journal <- journalForUnit (unitName unit) sinceTimestamp
              return $ M.insert (unitName unit) journal m

journalForUnit :: UnitName -> LocalTime -> IO Text
journalForUnit (UnitName name) sinceTimestamp = do
    let args = [ "-u", name, "-S", ts ]
        ts = timestamp sinceTimestamp

    readStdoutOrDie  "/usr/bin/journalctl" args ""

timestamp :: LocalTime -> String
timestamp = formatTime defaultTimeLocale "%F %T"

