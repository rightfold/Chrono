{-# LANGUAGE TemplateHaskell #-}

module Language.PHP.Pretty
  ( -- * Namespaces
    InitialBackslash (..)
  , prettyNamespaceName
  , prettyNamespace

    -- * Statements and expressions
  , prettyStatement
  , prettyExpression
  , prettyExpression'
  , parenthesizeExpression

    -- * Classes
  , prettyClassMethod

    -- * Optics
  , _NoInitialBackslash
  , _InitialBackslash
  ) where

import Language.PHP.Syntax

import Control.Lens ((^.), makePrisms, view)
import Data.Foldable.Extra (intercalate)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)

import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Vector as Vector

indent :: Builder -> Builder
indent = id

t :: Text -> Builder
t = Builder.fromText



data InitialBackslash
  = NoInitialBackslash
  | InitialBackslash
  deriving (Enum, Eq, Ord, Show)

prettyNamespaceName :: InitialBackslash -> NamespaceName -> Builder
prettyNamespaceName InitialBackslash =
  foldMap ("\\" <>) . fmap t . view _NamespaceName
prettyNamespaceName NoInitialBackslash =
  intercalate "\\" . fmap t . Vector.toList . view _NamespaceName

prettyNamespace :: Namespace -> Builder
prettyNamespace namespace =
  "namespace" <> prettyNamespaceName NoInitialBackslash (namespace ^. name) <> "\n" <>
  "{\n" <>
  indent (foldMap prettyStatement (namespace ^. body)) <>
  "}\n"



prettyStatement :: Statement -> Builder
prettyStatement (ClassStatement className classMethods) =
  "final class " <> t className <> " {\n" <>
  indent (
      "private function __construct() {\n" <>
      "}\n" <>
      foldMap prettyClassMethod classMethods
  ) <>
  "}\n"
prettyStatement (ReturnStatement returnValue) =
  "return " <> prettyExpression returnValue <> ";\n"

prettyExpression :: Expression -> Builder
prettyExpression = prettyExpression' . parenthesizeExpression

-- | Like 'prettyExpression', but without inserting parentheses where necessary.
prettyExpression' :: Expression -> Builder
prettyExpression' (ConstantExpression namespaceName constantName) =
  prettyNamespaceName InitialBackslash namespaceName <> "\\" <> t constantName
prettyExpression' (VariableExpression variableName) =
  "$" <> t variableName
prettyExpression' (StaticMethodCallExpression classNamespace className methodName arguments) =
  prettyNamespaceName InitialBackslash classNamespace <> "\\" <> t className <>
  "::" <> t methodName <>
  "(" <> intercalate ", " (prettyExpression' <$> arguments) <> ")"

parenthesizeExpression :: Expression -> Expression
parenthesizeExpression = id



prettyClassMethod :: ClassMethod -> Builder
prettyClassMethod classMethod =
  "public static function " <> t (classMethod ^. name) <> "() {\n" <>
  indent (
      foldMap prettyStatement (classMethod ^. body)
  ) <>
  "}\n"



$(makePrisms ''InitialBackslash)
