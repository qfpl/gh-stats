{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module GhStats.Test where

import           Control.Exception      (throw)
import           Control.Monad.Except   (ExceptT, runExceptT)
import           Control.Monad.Morph    (hoist)
import           Control.Monad.Reader   (ReaderT, runReaderT)
import           Database.SQLite.Simple (Connection)

import           Hedgehog               (PropertyT)

import           GhStats.Types          (Error)

type GhStatsTestM a =
  PropertyT (ReaderT Connection (ExceptT Error IO)) a

runGhStatsTestM ::
  Connection
  -> GhStatsTestM a
  -> PropertyT IO a
runGhStatsTestM conn =
  hoist $ \rem -> fmap (either throw id) . runExceptT . runReaderT rem $ conn
