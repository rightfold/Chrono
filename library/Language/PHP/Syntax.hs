{-# LANGUAGE TemplateHaskell #-}

module Language.PHP.Syntax
  ( -- * Namespaces
    NamespaceName (..)
  , Namespace (..)
  , globalNamespaceName

    -- * Statements and expressions
  , Statement (..)
  , Expression (..)

    -- * Functions
  , ClassMethod (..)
  , FunctionParameter (..)

    -- * Type hints
  , TypeHint (..)

    -- * Optics
  , HasBody (..)
  , HasName (..)
  , HasParameters (..)
  , HasReturnTypeHint (..)
  , HasTypeHint (..)
  , _BoolTypeHint
  , _ClassStatement
  , _ConstantExpression
  , _NamespaceName
  , _ReturnStatement
  ) where

import Control.Lens (makeFields, makePrisms)
import Data.Text (Text)
import Data.Vector (Vector)



newtype NamespaceName =
  NamespaceName (Vector Text)
  deriving stock (Eq, Ord, Show)

data Namespace =
  Namespace
    { _namespaceName :: NamespaceName
    , _namespaceBody :: Vector Statement }
  deriving stock (Eq, Show)

globalNamespaceName :: NamespaceName
globalNamespaceName = NamespaceName []



data Statement
  = ClassStatement Text (Vector ClassMethod)
  | ReturnStatement Expression
  deriving stock (Eq, Show)

data Expression
  = ConstantExpression NamespaceName Text
  | VariableExpression Text
  | StaticMethodCallExpression NamespaceName Text Text (Vector Expression)
  deriving stock (Eq, Show)



data ClassMethod =
  ClassMethod
    { _classMethodName :: Text
    , _classMethodParameters :: Vector FunctionParameter
    , _classMethodReturnTypeHint :: Maybe TypeHint
    , _classMethodBody :: Vector Statement }
  deriving stock (Eq, Show)

data FunctionParameter =
  FunctionParameter
    { _functionParameterName :: Text
    , _functionParameterTypeHint :: Maybe TypeHint }
  deriving stock (Eq, Show)



data TypeHint
  = BoolTypeHint
  deriving stock (Eq, Show)



$(makeFields ''ClassMethod)
$(makeFields ''FunctionParameter)
$(makeFields ''Namespace)
$(makePrisms ''Expression)
$(makePrisms ''NamespaceName)
$(makePrisms ''Statement)
$(makePrisms ''TypeHint)
