{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Acid (loadMap, updateLastUpdate) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import Data.Time.LocalTime (ZonedTime(..))
import Data.Typeable

import qualified Data.Map as Map

-- Key/Value example copied from acid-state example: https://github.com/acid-state/acid-state/blob/master/examples/KeyValue.hs

type Key   = String
type Value = Maybe ZonedTime

data KeyValue = KeyValue !(Map.Map Key Value) deriving (Typeable)

$(deriveSafeCopy 0 'base ''KeyValue)

insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value = do
    KeyValue m <- get
    put (KeyValue (Map.insert key value m))

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key = do
    KeyValue m <- ask
    return (Map.lookup key m)

getMapInternal :: Query KeyValue (Map.Map Key Value)
getMapInternal = do
    KeyValue m <- ask
    return m

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey, 'getMapInternal])

loadMap :: FilePath -> IO (Map.Map Key Value)
loadMap fp = do
    acid <- openLocalStateFrom fp (KeyValue Map.empty)
    m <- query acid GetMapInternal
    closeAcidState acid
    return m

updateLastUpdate :: FilePath -> String -> Maybe ZonedTime -> IO ()
updateLastUpdate fp hash lastUpdate = do
    acid <- openLocalStateFrom fp (KeyValue Map.empty)
    _ <- update acid (InsertKey hash lastUpdate)
    closeAcidState acid
