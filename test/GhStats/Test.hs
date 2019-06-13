{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module GhStats.Test where

import           Control.Exception      (throw)
import           Control.Monad.Except   (ExceptT, runExceptT)
import           Control.Monad.Morph    (hoist)
import           Control.Monad.Reader   (ReaderT, runReaderT)
import           Database.SQLite.Simple (Connection)

import           Hedgehog               (PropertyT)

import           GhStats.Types          (Error)

newtype GhStatsTestM a =
  GhStatsTestM (PropertyT (ReaderT Connection (ExceptT Error IO)) a)
  deriving (Functor, Applicative, Monad)

runGhStatsTestM ::
  Connection
  -> GhStatsTestM a
  -> PropertyT IO a
runGhStatsTestM conn (GhStatsTestM rem) =
  fmap (either throw id) . runExceptT . runReaderT rem $ conn
