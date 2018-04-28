-- | Extra folds.
module Control.Foldl.Extra where

import Control.Foldl (Fold (..))
import Data.Profunctor (lmap)
import Data.Map (Map)
import Data.Set (Set)

import qualified Control.Foldl as Fold
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Collect unique keys, or fail if there are any duplicate keys.
collectUnique :: Ord k => Fold (k, v) (Either (Set k) (Map k v))
collectUnique = do
  duplicates <- lmap fst dropUnique
  results <- Fold.map
  pure $ if not (Set.null duplicates)
           then Left duplicates
           else Right results
{-# INLINABLE collectUnique #-}

-- | Keep only duplicate elements.
dropUnique :: Ord k => Fold k (Set k)
dropUnique = Map.keysSet . Map.filter (> 1) <$> countEach
{-# INLINABLE dropUnique #-}

-- | Count how many times each element occurs.
countEach :: Ord k => Fold k (Map k Word)
countEach = Fold step Map.empty id
  where step m x = Map.alter (Just . maybe 1 (+ 1)) x m
{-# INLINABLE countEach #-}
