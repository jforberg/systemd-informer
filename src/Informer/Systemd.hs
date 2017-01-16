{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Informer.Systemd where

import Control.Monad
import qualified Data.Bimap as Bimap
import Data.Time
import qualified Data.Map.Strict as Map
import Data.List (sort)
import Data.Int
import Data.Word
import Data.Maybe
import DBus
import DBus.Client

-- from systemd: src/code/dbus-manager.c
data Unit = Unit
    { unitName :: String
    , unitDescription :: String
    , unitLoaded :: LoadedState
    , unitActive :: ActiveState
    , unitSub :: String
    , unitFollowed :: String
    , unitPath :: ObjectPath
    }
    deriving (Show)

instance Eq Unit where
    u1 == u2 = unitPath u1 == unitPath u2

-- from systemd: src/core/unit.h
data ActiveState = UnitActive
                 | UnitInactive
                 | UnitFailed
                 | UnitActivating
                 | UnitDeactivating
    deriving (Eq, Ord)

activeStateBimap = Bimap.fromList
    [ (UnitActive, "active")
    , (UnitInactive, "inactive")
    , (UnitFailed, "failed")
    , (UnitActivating, "activating")
    , (UnitDeactivating, "deactivating")
    ]

instance Show ActiveState where
    show = (activeStateBimap Bimap.!)

activeStateFromString :: String -> ActiveState
activeStateFromString = (activeStateBimap Bimap.!>)

--from systemd: src/basic/unit-name.h
data LoadedState = UnitStub
                 | UnitLoaded
                 | UnitNotFound
                 | UnitError
                 | UnitMerged
                 | UnitMasked
    deriving (Eq, Ord)

loadedStateBimap = Bimap.fromList
    [ (UnitStub, "stub")
    , (UnitLoaded, "loaded")
    , (UnitNotFound, "not-found")
    , (UnitError, "error")
    , (UnitMerged, "merged")
    , (UnitMasked, "masked")
    ]

instance Show LoadedState where
    show = (loadedStateBimap Bimap.!)

loadedStateFromString :: String -> LoadedState
loadedStateFromString = (loadedStateBimap Bimap.!>)

data JobInfo = JobInfo
    { jobInfoId :: Word32
    , jobInfoPath :: ObjectPath
    , jobInfoUnitName :: String
    , jobInfoResult :: String
    }
    deriving (Show)

systemdBus = "org.freedesktop.systemd1" :: BusName
systemdObject = "/org/freedesktop/systemd1" :: ObjectPath
managerInterface = "org.freedesktop.systemd1.Manager" :: InterfaceName
unitInterface = "org.freedesktop.systemd1.Unit" :: InterfaceName
propertiesInterface = "org.freedesktop.DBus.Properties" :: InterfaceName

listUnits client = do
    reply <- managerCall client "ListUnits" []
    let Just variant = fromVariant (methodReturnBody reply !! 0) :: Maybe Array
    return $ map unitFromVariant $ arrayItems variant

unitFromVariant variant =
    let (n, d, l, a, s, f, p, ji :: Word32, jt :: String, jp :: ObjectPath) = fromJust . fromVariant $ variant
    in Unit { unitName = n
            , unitDescription = d
            , unitLoaded = loadedStateFromString l
            , unitActive = activeStateFromString a
            , unitSub = s
            , unitFollowed = f
            , unitPath = p
            }

getKernelTimestamp :: Client -> IO LocalTime
getKernelTimestamp client = do
    reply <- getProperty client systemdObject managerInterface "KernelTimestamp" systemdBus

    let Just variant = fromVariant (methodReturnBody reply !! 0) :: Maybe Variant
        Just timestamp = fromVariant variant :: Maybe Word64
        epochSeconds = timestamp `quot` 1000000
        zonedtime = parseTimeOrError False defaultTimeLocale "%s" $ show epochSeconds

    return $ zonedTimeToLocalTime zonedtime

registerJobHandler client handler = do
    let matchRule = matchAny { matchInterface = Just managerInterface
                             , matchPath = Just systemdObject
                             , matchMember = Just "JobRemoved"
                             }
        wrappedHandler = handler . jobInfoFromVariants . signalBody

    void $ addMatch client matchRule wrappedHandler

jobInfoFromVariants variants =
    JobInfo { jobInfoId = get 0
            , jobInfoPath = get 1
            , jobInfoUnitName = get 2
            , jobInfoResult = get 3
            }
    where get n = fromJust . fromVariant $ variants !! n

getUnit :: Client -> String -> IO Unit
getUnit client unitName = do
    unitPath <- getUnitPath client unitName
    reply <- getAllProperties client unitPath unitInterface systemdBus
    let Just variant = fromVariant (methodReturnBody reply !! 0) :: Maybe (Map.Map String Variant)
    return $ unitFromVariantMap unitPath variant

getUnitPath client unitName = do
    reply <- managerCall client "GetUnit" [toVariant unitName]
    let Just path = fromVariant (methodReturnBody reply !! 0) :: Maybe ObjectPath
    return path

unitFromVariantMap :: ObjectPath -> Map.Map String Variant -> Unit
unitFromVariantMap path map =
    Unit { unitName = get "Id"
         , unitDescription = get "Description"
         , unitLoaded = loadedStateFromString $ get "LoadState"
         , unitActive = activeStateFromString $ get "ActiveState"
         , unitSub = get "SubState"
         , unitFollowed = get "Following"
         , unitPath = path
         }
    where get k = let Just val = fromVariant $ map Map.! k in val

managerCall :: Client -> MemberName -> [Variant] -> IO MethodReturn
managerCall client method arguments =
    call_ client (methodCall systemdObject managerInterface method)
        { methodCallDestination = Just systemdBus
        , methodCallBody = arguments
        }

getManagerProperty client property =
    getProperty client systemdObject managerInterface property systemdBus

getProperty :: Client -> ObjectPath -> InterfaceName -> MemberName -> BusName -> IO MethodReturn
getProperty client object interface property dest =
    call_ client (methodCall object propertiesInterface "Get")
        { methodCallDestination = Just dest
        , methodCallBody = [toVariant (interface), toVariant (property)]
        }

getAllProperties :: Client -> ObjectPath -> InterfaceName -> BusName -> IO MethodReturn
getAllProperties client object interface dest =
    call_ client (methodCall object propertiesInterface "GetAll")
        { methodCallDestination = Just dest
        , methodCallBody = [toVariant (interface)]
        }
