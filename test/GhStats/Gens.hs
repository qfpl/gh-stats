{-# LANGUAGE DataKinds #-}

module GhStats.Gens where

import           Hedgehog         (Gen, GenT, MonadGen, MonadTest, Property,
                                   PropertyT, annotateShow, evalEither, evalM,
                                   failure, forAll, property, success, tripping,
                                   (===))
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range

import qualified Data.Map         as M
import           Data.Time        (UTCTime (UTCTime), fromGregorian,
                                   secondsToDiffTime)
import qualified Data.Vector      as V
import qualified GitHub           as GH

import           GhStats.Db.Types (DbRepoStats (DbRepoStats, _dbRepoStatsId, _dbRepoStatsName),
                                   HasTable (tableName, tableNameQ), Id (Id),
                                   Pop (Pop, popId, popRepoId),
                                   Position (Position),
                                   VC (VC, _vcId, _vcRepoId, _vcRepoName))
import           GhStats.Types    (AsSQLiteResponse, Count (Count),
                                   Error (SQLiteError), Forks (..),
                                   GhStatsM (GhStatsM), HasConnection,
                                   RepoStats, Stars (..), Uniques (Uniques),
                                   runGhStatsM)

genDbRepoStats ::
   MonadGen m
   => m DbRepoStats
genDbRepoStats =
  DbRepoStats
  Nothing
  <$> genName
  <*> genUTCTime
  <*> (Stars <$> genIntCount)
  <*> (Forks <$> genIntCount)
  <*> genCount
  <*> genUniques
  <*> genCount
  <*> genUniques

genId ::
  MonadGen m
  => m (Id a)
genId =
  Id <$> Gen.int64 Range.linearBounded

genName ::
  MonadGen m
  => m (GH.Name a)
genName =
  -- TODO: should probably change this to `unicode` once hedgehog is updated
  -- (https://github.com/hedgehogqa/haskell-hedgehog/pull/303).
  GH.mkName (undefined :: GH.Name a) <$> Gen.text (Range.linear 1 30) Gen.ascii

genUTCTime
  :: MonadGen n
  => n UTCTime
genUTCTime =
  let
    gYear = Gen.int (Range.linearFrom 1900 1970 2500)
    gMonth = Gen.int (Range.linear 1 12)
    -- fromGregorian automatically trims to valid dates, so 2001-02-31 becomes 2001-02-28
    gDay = Gen.int (Range.linear 1 31)
    hToS = (* 3600)
    gSeconds = Gen.int (Range.linearFrom (hToS 12) 0 86400)
    gUTCTimeDay = fromGregorian . fromIntegral <$> gYear <*> gMonth <*> gDay
    gDiffTime = secondsToDiffTime . fromIntegral <$> gSeconds
  in
    UTCTime <$> gUTCTimeDay <*> gDiffTime

genReferrer ::
  MonadGen m
  => m GH.Referrer
genReferrer =
  GH.Referrer <$> genName <*> genIntCount <*> genIntCount

-- List of referrers with unique names
genReferrers ::
  MonadGen m
  => m [GH.Referrer]
genReferrers =
  M.elems <$> Gen.map (Range.constant 0 15) ((\r -> (GH.referrer r, r)) <$> genReferrer)

genPath ::
  MonadGen m
  => m GH.PopularPath
genPath =
  let
    genText = Gen.text (Range.linear 1 100) Gen.ascii
  in
    GH.PopularPath <$> genText <*> genText <*> genIntCount <*> genIntCount

genPaths ::
  MonadGen m
  => m [GH.PopularPath]
genPaths =
  genUniqueList 0 15 genPath GH.popularPath

genUniqueList ::
  ( MonadGen m
  , Ord b
  )
  => Int
  -> Int
  -> m a
  -> (a -> b)
  -> m [a]
genUniqueList lower upper gen f =
  fmap M.elems . Gen.map (Range.constant lower upper) . fmap (\a -> (f a, a)) $ gen

genIntCount ::
  MonadGen m
  => m Int
genIntCount =
  Gen.int (Range.linear 0 1000000)

genPop ::
  MonadGen m
  => m (Pop a)
genPop =
  Pop
  Nothing
  <$> (Position <$> Gen.int (Range.linear 1 10))
  <*> genName
  <*> genCount
  <*> genUniques
  <*> genId

genVC ::
  MonadGen m
  => m (VC a)
genVC =
  VC Nothing
  <$> genUTCTime
  <*> genCount
  <*> genUniques
  <*> genId
  <*> genName

genCount ::
  MonadGen m
  => m (Count a)
genCount =
  Count <$> genIntCount

genUniques ::
  MonadGen m
  => m (Uniques a)
genUniques =
  Uniques <$> genIntCount

genGhViews ::
  MonadGen m
  => m GH.Views
genGhViews =
  GH.Views <$> genIntCount <*> genIntCount <*> genTrafficCountVector

genGhClones ::
  MonadGen m
  => m GH.Clones
genGhClones =
  GH.Clones <$> genIntCount <*> genIntCount <*> genTrafficCountVector

genTrafficCountVector ::
  MonadGen m
  => m (V.Vector (GH.TrafficCount a))
genTrafficCountVector =
  V.fromList <$> genUniqueList 14 14 genTrafficCount GH.trafficCountTimestamp

genTrafficCount ::
  MonadGen m
  => m (GH.TrafficCount a)
genTrafficCount =
  GH.TrafficCount <$> genUTCTime <*> genIntCount <*> genIntCount
