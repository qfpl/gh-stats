{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module GhStats.Test where

import           Control.Exception      (throw)
import           Control.Monad.Except   (ExceptT, runExceptT)
import           Control.Monad.Morph    (hoist)
import           Control.Monad.Reader   (ReaderT, runReaderT)
import           Database.SQLite.Simple (Connection)

import           Hedgehog               (PropertyT)

import           GhStats.Types          (Error)

type GhStatsPropReaderT = ReaderT Connection (ExceptT Error IO)

-- For use in properties, such that we can use runGhStatsPropertyT to get to a Property
type GhStatsPropertyT a = PropertyT GhStatsPropReaderT a

runGhStatsPropertyT ::
  Connection
  -> GhStatsPropertyT a
  -> PropertyT IO a
runGhStatsPropertyT conn =
  hoist (\rem -> fmap (either throw id) . runExceptT $ runReaderT rem conn)
