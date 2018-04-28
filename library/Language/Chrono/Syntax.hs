{-# LANGUAGE TemplateHaskell #-}

-- | Abstract syntax tree.
module Language.Chrono.Syntax where

import Control.Lens (makeFields, makePrisms)
import Data.Text (Text)
import Data.Vector (Vector)

-- | Name of a namespace.
newtype NamespaceName =
  NamespaceName (Vector Text)
  deriving stock (Eq, Ord, Show)

-- | Name of a subroutine.
data SubroutineName =
  SubroutineName
    { _subroutineNameNamespace :: NamespaceName
    , _subroutineNameName :: Text }
  deriving stock (Eq, Ord, Show)

-- | Name of a local variable.
newtype LocalVariableName =
  LocalVariableName Text
  deriving stock (Eq, Ord, Show)

-- | Subroutine parameter.
data SubroutineParameter =
  SubroutineParameter
    { _subroutineParameterName :: LocalVariableName
    , _subroutineParameterSchema :: Schema }
  deriving stock (Eq, Show)

-- | Constant.
data Constant
  -- | A Boolean constant.
  = BoolConstant Bool

  deriving stock (Eq, Show)

-- | Definition.
data Definition
  -- | Function definition.
  = FunctionDefinition SubroutineName (Vector SubroutineParameter) Schema Expression

  deriving stock (Eq, Show)

-- | Expression.
data Expression
  -- | Expression that evaluates to a constant.
  = ConstantExpression Constant

  -- | Expression that evaluates to the value of a local variable.
  | LocalVariableExpression LocalVariableName

  -- | Expression that calls a subroutine.
  | CallSubroutineExpression SubroutineName (Vector Expression)

  deriving stock (Eq, Show)

-- | Schema.
data Schema
  -- | Schema for Booleans.
  = BoolSchema

  deriving stock (Eq, Show)

$(makePrisms ''NamespaceName)
$(makeFields ''SubroutineName)
$(makePrisms ''LocalVariableName)
$(makeFields ''SubroutineParameter)
$(makePrisms ''Definition)
$(makePrisms ''Expression)
$(makePrisms ''Schema)
