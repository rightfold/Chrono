{-# LANGUAGE TemplateHaskell #-}

-- | Collect definitions.
module Language.Chrono.Collect where

import Control.Foldl (Fold)
import Control.Foldl.Extra (collectUnique)
import Control.Lens (makePrisms)
import Data.Map (Map)
import Data.Profunctor (lmap)
import Data.Set (Set)
import Data.Vector (Vector)
import Language.Chrono.Syntax (Definition (..), Expression, Schema, SubroutineName, SubroutineParameter)

-- | Subroutine.
data Subroutine
  = Function (Vector SubroutineParameter) Schema Expression
  deriving stock (Eq, Show)

-- | Collect subroutines so they can be used for type checking expressions. May
-- fail with a set of subroutines that were defined multiple times.
collectSubroutines :: Fold Definition (Either (Set SubroutineName)
                                              (Map SubroutineName Subroutine))
collectSubroutines = flip lmap collectUnique $ \case
  FunctionDefinition name parameters returnSchema body ->
    (name, Function parameters returnSchema body)

$(makePrisms ''Subroutine)
