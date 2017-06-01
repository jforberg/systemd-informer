{-# LANGUAGE OverloadedStrings #-}

module Informer.Report
( ReportData(..)
, formatReport
)
where

import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import Informer.Systemd
import Informer.Util

data ReportData = ReportData
    { rdFailedUnits :: [Unit]
    , rdHostname :: Text
    , rdKernelTimestamp :: LocalTime
    , rdJournals :: Map UnitName Text
    }

formatReport :: ReportData -> Text
formatReport rd = T.concat $ sequence formatters rd
    where formatters = [ formatHeaders
                       , formatSummary
                       , formatUnitList
                       ]

formatHeaders rd = formatStrict
    "From: {} <root@{}>\n\
    \Subject: {} degraded: {} failed\n\
    \\n"
    [ capitalise $ rdHostname rd
    , rdHostname rd
    , rdHostname rd
    , describeUnits $ rdFailedUnits rd
    ]

    where describeUnits [] = "no units"
          describeUnits [u] = T.pack $ unUnitName $ unitName u
          describeUnits _ = "several units"

formatSummary rd = formatStrict
    "{}\n\
    \    State: {}\n\
    \    Failed units: {}\n\
    \    Since: {}\n\
    \\n"
    [ rdHostname rd
    , systemState $ rdFailedUnits rd
    , showText . length $ rdFailedUnits rd
    , uptimeSince $ rdKernelTimestamp rd
    ]

formatUnitList rd = T.intercalate "\n\n" entries
    where units = rdFailedUnits rd
          entries = map (formatUnit rd) units

formatUnit rd unit = T.concat [ formatUnitHeader unit
                                      , formatUnitJournal rd unit
                                      ]

formatUnitHeader :: Unit -> Text
formatUnitHeader unit = formatStrict "{} ({})\n    {} {} {}\n\n"
    [ unUnitName $ unitName unit
    , unitDescription unit
    , show $ unitLoaded unit
    , show $ unitActive unit
    , unitSub unit
    ]

formatUnitJournal rd unit = rdJournals rd Map.! unitName unit

uptimeSince timestamp = T.pack $ formatTime defaultTimeLocale "%F %T" timestamp

systemState failedUnits | null failedUnits   = "running"
                        | otherwise          = "degraded"

capitalise t = case T.uncons t of
    Nothing -> ""
    Just (c, cs) -> T.cons (toUpper c) cs
