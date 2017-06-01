{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Informer.Systemd
( Unit(..)
, UnitName(..)
, unUnitName
, ActiveState(..)
, LoadedState(..)
, JobInfo(..)
, listUnits
, getUnit
, getKernelTimestamp
, registerJobHandler
, removeJobHandler
)
where

import qualified Data.Bimap as Bimap
import Data.Time
import qualified Data.Map.Strict as Map
import Data.Word
import Data.Maybe
import Data.Text (Text)
import DBus
import DBus.Client

-- from systemd: src/code/dbus-manager.c
data Unit = Unit
    { unitName :: UnitName
    , unitDescription :: String
    , unitLoaded :: LoadedState
    , unitActive :: ActiveState
    , unitSub :: String
    , unitFollowed :: String
    , unitPath :: ObjectPath
    }
    deriving (Show, Eq)

newtype UnitName = UnitName String
    deriving (Show, Eq, Ord)

unUnitName (UnitName un) = un

-- from systemd: src/core/unit.h
data ActiveState = UnitActive
                 | UnitInactive
                 | UnitFailed
                 | UnitActivating
                 | UnitDeactivating
    deriving (Eq, Ord)

instance Show ActiveState where
    show = (activeStateBimap Bimap.!)

activeStateBimap = Bimap.fromList
    [ (UnitActive, "active")
    , (UnitInactive, "inactive")
    , (UnitFailed, "failed")
    , (UnitActivating, "activating")
    , (UnitDeactivating, "deactivating")
    ]

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
    , jobInfoUnitName :: UnitName
    , jobInfoResult :: Text
    }
    deriving (Show)

systemdBus = "org.freedesktop.systemd1" :: BusName
systemdObject = "/org/freedesktop/systemd1" :: ObjectPath
managerInterface = "org.freedesktop.systemd1.Manager" :: InterfaceName
unitInterface = "org.freedesktop.systemd1.Unit" :: InterfaceName
propertiesInterface = "org.freedesktop.DBus.Properties" :: InterfaceName

listUnits c = do
    reply <- managerCall c "ListUnits" []
    let Just variant = fromVariant (head $ methodReturnBody reply) :: Maybe Array
    return $ map unitFromVariant $ arrayItems variant

unitFromVariant variant =
    let (n, d, l, a, s, f, p, _ :: Word32, _ :: String, _ :: ObjectPath) = fromJust . fromVariant $ variant
    in Unit { unitName = UnitName n
            , unitDescription = d
            , unitLoaded = loadedStateFromString l
            , unitActive = activeStateFromString a
            , unitSub = s
            , unitFollowed = f
            , unitPath = p
            }

getKernelTimestamp :: Client -> IO LocalTime
getKernelTimestamp c = do
    reply <- getProperty c systemdObject managerInterface "KernelTimestamp" systemdBus

    let Just variant = fromVariant (head $ methodReturnBody reply) :: Maybe Variant
        Just timestamp = fromVariant variant :: Maybe Word64
        epochSeconds = timestamp `quot` 1000000
        zonedtime = parseTimeOrError False defaultTimeLocale "%s" $ show epochSeconds

    return $ zonedTimeToLocalTime zonedtime

registerJobHandler :: Client -> (JobInfo -> IO ()) -> IO SignalHandler
registerJobHandler c handler = do
    let matchRule = matchAny { matchInterface = Just managerInterface
                             , matchPath = Just systemdObject
                             , matchMember = Just "JobRemoved"
                             }
        wrappedHandler = handler . jobInfoFromVariants . signalBody

    addMatch c matchRule wrappedHandler

removeJobHandler :: Client -> SignalHandler -> IO ()
removeJobHandler = removeMatch

jobInfoFromVariants variants =
    JobInfo { jobInfoId = get 0
            , jobInfoPath = get 1
            , jobInfoUnitName = UnitName $ get 2
            , jobInfoResult = get 3
            }
    where get n = fromJust . fromVariant $ variants !! n

getUnit :: Client -> UnitName -> IO Unit
getUnit c name = do
    path <- getUnitPath c name
    reply <- getAllProperties c path unitInterface systemdBus
    let Just variant = fromVariant (head $ methodReturnBody reply) :: Maybe (Map.Map String Variant)
    return $ unitFromVariantMap path variant

getUnitPath c (UnitName name) = do
    reply <- managerCall c "GetUnit" [toVariant name]
    let Just path = fromVariant (head $ methodReturnBody reply) :: Maybe ObjectPath
    return path

unitFromVariantMap :: ObjectPath -> Map.Map String Variant -> Unit
unitFromVariantMap path m =
    Unit { unitName = UnitName $ get "Id"
         , unitDescription = get "Description"
         , unitLoaded = loadedStateFromString $ get "LoadState"
         , unitActive = activeStateFromString $ get "ActiveState"
         , unitSub = get "SubState"
         , unitFollowed = get "Following"
         , unitPath = path
         }
    where get k = let Just val = fromVariant $ m Map.! k in val

managerCall :: Client -> MemberName -> [Variant] -> IO MethodReturn
managerCall c meth args =
    call_ c (methodCall systemdObject managerInterface meth)
        { methodCallDestination = Just systemdBus
        , methodCallBody = args
        }

getProperty :: Client -> ObjectPath -> InterfaceName -> MemberName -> BusName -> IO MethodReturn
getProperty c object interface property dest =
    call_ c (methodCall object propertiesInterface "Get")
        { methodCallDestination = Just dest
        , methodCallBody = [ toVariant interface, toVariant property ]
        }

getAllProperties :: Client -> ObjectPath -> InterfaceName -> BusName -> IO MethodReturn
getAllProperties c object interface dest =
    call_ c (methodCall object propertiesInterface "GetAll")
        { methodCallDestination = Just dest
        , methodCallBody = [ toVariant interface ]
        }
