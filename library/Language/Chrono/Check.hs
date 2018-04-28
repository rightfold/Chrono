{-# LANGUAGE TemplateHaskell #-}

-- | Type-check expressions.
module Language.Chrono.Check
  ( -- * Errors
    Error (..)

    -- * Subroutines
  , CheckSubroutine
  , SubroutineEnvironment (..)
  , checkSubroutine

    -- * Expressions
  , CheckExpression
  , ExpressionEnvironment (..)
  , checkExpression

    -- * Constants
  , checkConstant

    -- * Types
  , assertCoercibleFrom

    -- * Optics
  , subroutines
  , localVariables
  , lookupSubroutine
  , lookupLocalVariable
  ) where

import Prelude hiding (error)

import Control.Applicative (liftA2)
import Control.Lens ((^.), at, makeFields, view)
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Reader (ReaderT, withReaderT)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Vector (Vector)
import Language.Chrono.Collect (Subroutine (..))
import Language.Chrono.Syntax (Constant (..), Expression (..), LocalVariableName, Schema (..), SubroutineName, name, schema)

import qualified Data.Map as Map
import qualified Data.Vector as Vector



-- | A type-checking error.
data Error
  = LocalVariableDoesNotExist LocalVariableName
  | SubroutineDoesNotExist SubroutineName
  | InvalidSubroutineArgumentCount
  deriving stock (Eq, Show)



-- | Monad for type-checking subroutines in.
type CheckSubroutine =
  ReaderT SubroutineEnvironment (Either Error)

-- | Environment for type-checking subroutines in.
data SubroutineEnvironment =
  SubroutineEnvironment
    { _subroutineEnvironmentSubroutines ::
        Map SubroutineName (Vector Schema, Schema) }



-- | Monad for type-checking expressions in.
type CheckExpression =
  ReaderT ExpressionEnvironment (Either Error)

-- | Environment for type-checking expressions in.
data ExpressionEnvironment =
  ExpressionEnvironment
    { _expressionEnvironmentSubroutines ::
        Map SubroutineName (Vector Schema, Schema)
    , _expressionEnvironmentLocalVariables ::
        Map LocalVariableName Schema }



$(makeFields ''SubroutineEnvironment)
$(makeFields ''ExpressionEnvironment)

lookupSubroutine
  :: ( MonadError Error m
     , MonadReader e m
     , HasSubroutines e
         (Map SubroutineName a) )
  => SubroutineName
  -> m a
lookupSubroutine subroutineName =
  maybe (throwError error) pure =<<
    view (subroutines . at subroutineName)
  where error = SubroutineDoesNotExist subroutineName

lookupLocalVariable
  :: ( MonadError Error m
     , MonadReader e m
     , HasLocalVariables e
         (Map LocalVariableName a) )
  => LocalVariableName
  -> m a
lookupLocalVariable localVariableName =
  maybe (throwError error) pure =<<
    view (localVariables . at localVariableName)
  where error = LocalVariableDoesNotExist localVariableName



-- | Type-check a subroutine.
checkSubroutine :: Subroutine -> CheckSubroutine ()
checkSubroutine (Function parameters returnSchema body) = do
  bodySchema <- withReaderT bodyEnvironment (checkExpression body)
  returnSchema `assertCoercibleFrom` bodySchema
  where
    bodyEnvironment :: SubroutineEnvironment
                    -> ExpressionEnvironment
    bodyEnvironment e =
      ExpressionEnvironment
        { _expressionEnvironmentSubroutines = e ^. subroutines
        , _expressionEnvironmentLocalVariables = bodyEnvironmentParameters }

    bodyEnvironmentParameters
      :: Map LocalVariableName Schema
    bodyEnvironmentParameters =
      Map.fromList . fmap (liftA2 (,) (^. name) (^. schema)) $
        Vector.toList parameters

-- | Type-check an expression and return its schema.
checkExpression :: Expression -> CheckExpression Schema

checkExpression (ConstantExpression constant) =
  pure $ checkConstant constant

checkExpression (LocalVariableExpression localVariableName) =
  lookupLocalVariable localVariableName

checkExpression (CallSubroutineExpression subroutineName arguments) = do
  (parameterSchemas, returnSchema) <- lookupSubroutine subroutineName
  argumentSchemas <- traverse checkExpression arguments

  when (Vector.length parameterSchemas /=
        Vector.length argumentSchemas) $
    throwError InvalidSubroutineArgumentCount

  for_ (parameterSchemas `Vector.zip` argumentSchemas) $
    \(parameterSchema, argumentSchema) ->
      parameterSchema `assertCoercibleFrom` argumentSchema

  pure returnSchema



-- | Return the schema of a constant.
checkConstant :: Constant -> Schema
checkConstant (BoolConstant _) = BoolSchema



-- | Check that the first schema can be coerced into from the second schema.
assertCoercibleFrom :: MonadError Error m => Schema -> Schema -> m ()
assertCoercibleFrom BoolSchema BoolSchema = pure ()
