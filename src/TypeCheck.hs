{-# LANGUAGE OverloadedStrings #-}

module TypeCheck
       ( inferInputType
       , ensureOneFreeOrIdentInEachStep
       , unifySolution
       , unify
       , TypeError(..)
       ) where

import Builtins (Context, identType)
import Data.Text
import qualified Ast
import qualified Type
import qualified Data.Set as S

type Env = Maybe Type.Type

data TypeError
  = IdentifierIsNotDefined Text
  | FloatingLambdaCannotReturn Type.Type
  | IdentifierNotAFunctionOfAList Text Text Type.Type
  | NotAFunction Text Type.Type
  | CouldNotInferTypeOfFreeVariableInputIn Ast.Value
  | StepNMustBeIdentiferOrContainSingleFree Int Ast.Value
  --                       resolved expectedType actualType value
  | UnificationFailure Int Env      Type.Type    Type.Type  Ast.Value
  deriving (Show, Eq)

type Result a = Either TypeError a

--                          solution        expectedInputType       outputType
unifySolution :: Context -> Ast.Solution -> Type.Type            -> Result Type.Type
unifySolution context (Ast.Pipe s1 s2) it = do
  ot <- unifySolution context s1 it
  unifySolution context s2 ot

unifySolution context (Ast.For cond gen reduce) (Type.List it) = do
  condot <- unifyLambda context cond it
  assertTypeIs condot Type.Boolean (UnificationFailure 1 Nothing Type.Boolean condot $ Ast.body cond)
  genot <- unifyLambda context gen it
  reduceot <- unifyLambda context reduce it
  assertTypeIs genot reduceot (UnificationFailure 2 Nothing genot reduceot $ Ast.body reduce)
  pure $ Type.List genot

unifySolution context (Ast.FloatingLambda lambda) Type.Grid = do
  ot <- unifyLambda context lambda Type.CellState
  case ot of
    Type.Arrow Type.Grid ot -> pure ot
    _ -> error "TODO non-unifying grid functions"

unifySolution context (Ast.FloatingLambda lambda) (Type.List it) = do
  ot <- unifyLambda context lambda it
  case ot of
    Type.Number                         -> pure $ Type.List Type.Number
    Type.Boolean                        -> pure $ Type.List it
    Type.Arrow (Type.List (Type.Var a)) (Type.Var b) ->
      if a == b
      then pure it
      else Left $ FloatingLambdaCannotReturn ot
    Type.Arrow (Type.List (Type.Var a)) (Type.List (Type.Var b)) ->
      if a == b
      then pure $ Type.List it
      else Left $ FloatingLambdaCannotReturn ot
    Type.Arrow (Type.List a) (Type.List b) ->
      pure $ Type.List b
    Type.Arrow Type.Grid Type.Grid ->
      pure Type.Grid
    Type.Arrow (Type.List finElem) fout ->
      if finElem == it
      then pure fout
      else Left $ FloatingLambdaCannotReturn ot

    _ -> Left $ FloatingLambdaCannotReturn ot

unifySolution context (Ast.FloatingLambda (Ast.Body b)) t = do
  actual <- typeOf context b
  Left $ UnificationFailure 6 Nothing t actual b

unifySolution _ _ _ = error "TODO unification error for other cases..."

assertTypeIs :: Type.Type -> Type.Type -> TypeError -> Result ()
assertTypeIs a b err =
  if a == b
  then pure ()
  else Left err

--                        lambda        expectedInputType outputType
unifyLambda :: Context -> Ast.Lambda -> Type.Type ->      Result Type.Type
unifyLambda context (Ast.Body body) it = do
  ot <- typeOf context body
  _  <- unify context body ot (Just it)
  pure ot

unifyBinOp :: Context -> Ast.Value -> Type.Type -> Ast.Value -> Type.Type -> Env -> Result Env
unifyBinOp context a ta b tb env = do
  env' <- unify context a ta env
  unify context b tb env'

unify :: Context -> Ast.Value -> Type.Type -> Maybe Type.Type -> Result Env
unify _ (Ast.Inte _) Type.Number env = Right env
unify context (Ast.Subtract a b) Type.Number env =
  unifyBinOp context a Type.Number b Type.Number env

unify context (Ast.And a b) Type.Boolean env =
  unifyBinOp context a Type.Boolean b Type.Boolean env

unify context (Ast.Gt a b) Type.Boolean env =
  unifyBinOp context a Type.Number b Type.Number env

unify context (Ast.Divide a b) Type.Number env =
  unifyBinOp context a Type.Number b Type.Number env

unify context ast@(Ast.Identifier name) t env =
  case identType name context of
    Just t' ->
      if t == t'
      then Right env
      else Left $ UnificationFailure 3 env t t' ast
    Nothing ->
      case env of
        Nothing -> Right (Just t)
        Just t' ->
          if t == t'
          then Right (Just t)
          else Left $ UnificationFailure 4 env t' t ast

unify context ast@(Ast.Application fn arg) t env =
  case identType fn context of
    Just (Type.Arrow it ot) -> do
      _ <- unify context arg it env
      if ot == t
      then pure (Just ot)
      else Left $ UnificationFailure 7 env ot t ast
    Just t' ->
      Left $ NotAFunction fn t'
    Nothing ->
      Left $ IdentifierIsNotDefined fn

unify context ast t env = do
  t' <- typeOf context ast
  if t' == t
  then Right env
  else Left $ UnificationFailure 5 env t t' ast

typeOf :: Context -> Ast.Value -> Result Type.Type
typeOf _ (Ast.Inte _)          = Right Type.Number
typeOf _ (Ast.Gt _ _)          = Right Type.Boolean
typeOf _ (Ast.And _ _)         = Right Type.Boolean
typeOf _ (Ast.Or _ _)          = Right Type.Boolean
typeOf _ (Ast.Equals _ _)      = Right Type.Boolean
typeOf _ (Ast.Divide _ _)      = Right Type.Number
typeOf _ (Ast.Subtract _ _)    = Right Type.Number
typeOf _ (Ast.Raised _ _)      = Right Type.Number
typeOf _ (Ast.Add _ _)         = Right Type.Number
typeOf context (Ast.Identifier name) =
  case identType name context of
    Nothing -> Left $ IdentifierIsNotDefined name
    Just t  -> Right t
typeOf context (Ast.Application name _) =
  case identType name context of
    Just (Type.Arrow _ ot) -> Right ot
    _ -> Left $ IdentifierIsNotDefined name

ensureOneFreeOrIdentInEachStep :: Context -> Ast.Solution -> Result ()
ensureOneFreeOrIdentInEachStep context = go 1 . unpipe
  where

    go n (Ast.For (Ast.Body l1) (Ast.Body l2) (Ast.Body l3):rest) =
      oneFreeOrIdent context n l1 >> oneFreeOrIdent context n l2 >> oneFreeOrIdent context n l3 >> go (n+1) rest

    go n (Ast.FloatingLambda (Ast.Body l):rest) =
      oneFreeOrIdent context n l >> go (n+1) rest

    go _ _ =
      pure ()

    oneFreeOrIdent :: Context -> Int -> Ast.Value -> Result ()
    oneFreeOrIdent _ _ (Ast.Identifier _) = pure ()
    oneFreeOrIdent context n ast =
      if S.size (frees context ast) /= 1
      then Left $ StepNMustBeIdentiferOrContainSingleFree n ast
      else pure ()

    frees :: Context -> Ast.Value -> S.Set Text
    frees context (Ast.Gt a b)          = S.union (frees context a) (frees context b)
    frees context (Ast.Divide a b)      = S.union (frees context a) (frees context b)
    frees context (Ast.Subtract a b)    = S.union (frees context a) (frees context b)
    frees context (Ast.Add a b)         = S.union (frees context a) (frees context b)
    frees context (Ast.Raised a b)      = S.union (frees context a) (frees context b)
    frees context (Ast.And a b)         = S.union (frees context a) (frees context b)
    frees context (Ast.Or a b)          = S.union (frees context a) (frees context b)
    frees context (Ast.Equals a b)      = S.union (frees context a) (frees context b)
    frees context (Ast.Inte _)          = S.empty
    frees context (Ast.Identifier name) =
      case identType name context of
        Nothing -> S.singleton name
        Just _ -> S.empty

    frees context (Ast.Application fn arg) =
      S.union (frees context (Ast.Identifier fn)) (frees context arg)

    unpipe (Ast.Pipe s1 s2) = unpipe s1 ++ unpipe s2
    unpipe v                = [v]

inferInputType :: Context -> Ast.Solution -> Result Type.Type
inferInputType context s = unifySolution context s (Type.List Type.Number)
