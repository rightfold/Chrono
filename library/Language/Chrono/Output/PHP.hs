module Language.Chrono.Output.PHP
  ( generateSubroutine
  , generateSubroutineParameter
  , generateExpression
  , generateConstant
  , generateSchema
  ) where

import Language.Chrono.Syntax

import Control.Lens ((&), (^.), view)
import Control.Monad.Cont (cont, runCont)
import Data.Vector (Vector)
import Language.Chrono.Collect (Subroutine (..))

import qualified Language.PHP.Syntax as PHP

-- | Generate a PHP namespace name from a namespace name.
generateNamespaceName :: NamespaceName -> PHP.NamespaceName
generateNamespaceName = PHP.NamespaceName . view _NamespaceName

-- | Generate a PHP namespace from a subroutine.
generateSubroutine :: SubroutineName -> Subroutine -> PHP.Namespace
generateSubroutine subroutineName (Function parameters returnSchema body) =
  let
    classStatement :: PHP.Statement
    classStatement = PHP.ClassStatement (subroutineName ^. name)
                                        [invokeClassMethod]

    invokeClassMethod :: PHP.ClassMethod
    invokeClassMethod = PHP.ClassMethod
      { PHP._classMethodName = subroutineName ^. name
      , PHP._classMethodParameters = generateSubroutineParameter <$> parameters
      , PHP._classMethodReturnTypeHint = generateSchema returnSchema
      , PHP._classMethodBody = invokeBody }

    invokeBody :: Vector PHP.Statement
    invokeBody = generateExpression body $
                   \result -> [PHP.ReturnStatement result]
  in
    PHP.Namespace (generateNamespaceName $ subroutineName ^. namespace)
                  [classStatement]

-- | Generate a PHP function parameter from a subroutine parameter.
generateSubroutineParameter :: SubroutineParameter -> PHP.FunctionParameter
generateSubroutineParameter subroutineParameter =
  PHP.FunctionParameter
    { PHP._functionParameterName = subroutineParameter ^. name . _LocalVariableName
    , PHP._functionParameterTypeHint = generateSchema (subroutineParameter ^. schema) }

-- | Generate PHP statements from an expression. The continuation is passed a
-- PHP expression that evaluates to the result.
generateExpression :: Expression -> (PHP.Expression -> Vector PHP.Statement)
                   -> Vector PHP.Statement
generateExpression (ConstantExpression constant) result =
  result $ generateConstant constant
generateExpression (LocalVariableExpression localVariableName) result =
  result $ PHP.VariableExpression (localVariableName ^. _LocalVariableName)
generateExpression (CallSubroutineExpression subroutineName arguments) result =
  flip runCont result $ do
    arguments' <- traverse (cont . generateExpression) arguments
    pure $ PHP.StaticMethodCallExpression
             (subroutineName ^. namespace & generateNamespaceName)
             (subroutineName ^. name)
             (subroutineName ^. name)
             arguments'

-- | Generate a PHP expression from a constant.
generateConstant :: Constant -> PHP.Expression
generateConstant (BoolConstant value) =
  PHP.ConstantExpression PHP.globalNamespaceName
                         (if value then "TRUE" else "FALSE")

-- | Generate a PHP type hint from a schema, if possible.
generateSchema :: Schema -> Maybe PHP.TypeHint
generateSchema BoolSchema = Just PHP.BoolTypeHint
