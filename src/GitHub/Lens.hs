{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Creates lenses for types in the GitHub package and maps field accessors to
-- names with leading underscores.
module GitHub.Lens where

import           Control.Lens        (makeLensesWith)
import           Data.Time.Clock     (UTCTime)
import           Data.Vector         (Vector)

import qualified GitHub              as GH

import           GitHub.Lens.Helpers (rulez)

_views :: GH.Views -> Vector (GH.TrafficCount 'GH.View)
_views = GH.views

_viewsCount :: GH.Views -> Int
_viewsCount = GH.viewsCount

_viewsUniques :: GH.Views -> Int
_viewsUniques = GH.viewsUniques

_trafficCountTimestamp :: GH.TrafficCount e -> UTCTime
_trafficCountTimestamp = GH.trafficCountTimestamp

_trafficCount :: GH.TrafficCount e -> Int
_trafficCount = GH.trafficCount

_trafficCountUniques :: GH.TrafficCount e -> Int
_trafficCountUniques = GH.trafficCountUniques

makeLensesWith rulez ''GH.Views
makeLensesWith rulez ''GH.TrafficCount
