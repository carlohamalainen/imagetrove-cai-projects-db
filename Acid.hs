{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Acid (loadSet, addExperiment) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import Data.Time.LocalTime (ZonedTime(..))
import Data.Typeable

import qualified Data.Map as Map

import Network.ImageTrove.Main
import Network.MyTardis.RestTypes

import qualified Data.Set as Set
import Data.Set (Set)

data ExperimentSet = ExperimentSet !(Set RestExperiment) deriving (Typeable)

$(deriveSafeCopy 0 'base ''RestParameter)
$(deriveSafeCopy 0 'base ''RestSchema)
$(deriveSafeCopy 0 'base ''RestExperimentParameterSet)
$(deriveSafeCopy 0 'base ''RestPermission)
$(deriveSafeCopy 0 'base ''RestGroup)
$(deriveSafeCopy 0 'base ''RestObjectACL)
$(deriveSafeCopy 0 'base ''RestExperiment)
$(deriveSafeCopy 0 'base ''ExperimentSet)

insertExperiment :: RestExperiment -> Update ExperimentSet ()
insertExperiment e = do
    ExperimentSet s <- get
    put $ ExperimentSet $ Set.insert e s

{-

isMember :: RestExperiment -> Query ExperimentSet Bool
isMember e = do
    ExperimentSet s <- get
    return True -- $ Set.member e s

Doesn't compile:

    Acid.hs:41:24:
        No instance for (MonadState ExperimentSet (Query ExperimentSet))
          arising from a use of `get'
        Possible fix:
          add an instance declaration for
          (MonadState ExperimentSet (Query ExperimentSet))
        In a stmt of a 'do' block: ExperimentSet s <- get
        In the expression:
          do { ExperimentSet s <- get;
               return True }
        In an equation for `isMember':
            isMember e
              = do { ExperimentSet s <- get;
                     return True }
-}

getSetInternal :: Query ExperimentSet (Set RestExperiment)
getSetInternal = do
    ExperimentSet s <- ask
    return s

$(makeAcidic ''ExperimentSet ['insertExperiment, 'getSetInternal])

loadSet :: FilePath -> IO (Set RestExperiment)
loadSet fp = do
    acid <- openLocalStateFrom fp (ExperimentSet Set.empty)
    m <- query acid GetSetInternal
    closeAcidState acid
    return m

addExperiment :: FilePath -> RestExperiment -> IO ()
addExperiment fp e = do
    acid <- openLocalStateFrom fp (ExperimentSet Set.empty)
    _ <- update acid (InsertExperiment e)
    closeAcidState acid
