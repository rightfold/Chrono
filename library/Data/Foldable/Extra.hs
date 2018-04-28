module Data.Foldable.Extra
  ( intercalate
  ) where

import Data.Foldable (fold, foldl')

intercalate :: forall f m. (Foldable f, Monoid m) => m -> f m -> m
intercalate sep = fold . foldl' step Nothing
  where step :: Maybe m -> m -> Maybe m
        step Nothing  x = Just x
        step (Just x) y = Just (x `mappend` sep `mappend` y)
{-# INLINABLE intercalate #-}
