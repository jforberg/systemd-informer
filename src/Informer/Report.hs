module Informer.Report
    ( ReportData(..)
    , formatReport
    ) where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Time
import Data.Word
import Text.Printf
import Text.Show

import Informer.Systemd

data ReportData = ReportData
    { reportFailedUnits :: [Unit]
    , reportHostname :: String
    , reportKernelTimestamp :: Word64
    , reportJournals :: Map.Map String String
    }

formatReport :: ReportData -> String
formatReport reportData =
    concatShows [ formatHeader reportData
                , formatUnitList reportData
                ] ""

formatHeader reportData = showString $ printf
    "%s\n\
    \    State: %s\n\
    \    Failed units: %d\n\
    \    Since: %s\n\
    \\n"
    (reportHostname reportData)
    (systemState $ reportFailedUnits reportData)
    (length $ reportFailedUnits reportData)
    (uptimeSince $ reportKernelTimestamp reportData)

formatUnitList reportData = concatShows $ map (formatUnit reportData) units
    where units = reportFailedUnits reportData

formatUnit reportData unit =
    concatShows [ formatUnitHeader unit
                , formatUnitJournal reportData unit
                ]

formatUnitHeader unit = showString $ printf "%s (%s)\n    %s %s %s\n\n"
    (unitName unit)
    (unitDescription unit)
    (show $ unitLoaded unit)
    (show $ unitActive unit)
    (unitSub unit)

formatUnitJournal reportData unit = showString . fromJust $ Map.lookup name journals
    where name = unitName unit
          journals = reportJournals reportData

uptimeSince timestamp = formatTime defaultTimeLocale "%F %T" time
    where time = parseTimeT $ show timestamp

parseTimeT str = parseTimeOrError False defaultTimeLocale "%s" str :: UTCTime

systemState failedUnits | failedUnits == []   = "running"
                        | otherwise           = "degraded"

concatShows = foldl (.) (showString "")
