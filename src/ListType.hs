{-# LANGUAGE OverloadedStrings #-}

module ListType
       ( inferInputType
       , inferOutputType
       , ensureOneFreeOrIdentInEachStep
       , unifySolution
       , unify
       , TypeError(..)
       ) where

import Builtins (identType)
import Data.Text
import qualified ListAst as Ast
import qualified Ast as Ast
import qualified Type as Type
import qualified Data.Set as S

type Env = Maybe Type.Type

data TypeError
  = IdentifierIsNotDefined Text
  | FloatingLambdaCannotReturn Type.Type
  | IdentifierNotAFunctionOfAList Text Text Type.Type
  | NotAFunction Text Text Type.Type
  | CouldNotInferTypeOfFreeVariableInputIn Ast.Value
  | StepNMustBeIdentiferOrContainSingleFree Int Ast.Value
  --                       resolved expectedType actualType value
  | UnificationFailure Int Env      Type.Type    Type.Type  Ast.Value
  deriving (Show, Eq)

type Result a = Either TypeError a

--               solution        expectedInputType       outputType
unifySolution :: Ast.Solution -> Type.Type     -> Result Type.Type
unifySolution (Ast.Pipe s1 s2) it = do
  ot <- unifySolution s1 it
  unifySolution s2 ot

unifySolution (Ast.For cond gen reduce) (Type.List it) = do
  condot <- unifyLambda cond it
  assertTypeIs condot Type.Boolean (UnificationFailure 1 Nothing Type.Boolean condot $ Ast.body cond)
  genot <- unifyLambda gen it
  reduceot <- unifyLambda reduce it
  assertTypeIs genot reduceot (UnificationFailure 2 Nothing genot reduceot $ Ast.body reduce)
  pure $ Type.List genot

unifySolution (Ast.FloatingLambda lambda) (Type.List it) = do
  ot <- unifyLambda lambda it
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
      if a == b
      then pure $ Type.List b
      else Left $ FloatingLambdaCannotReturn ot
    Type.Arrow (Type.List finElem) fout ->
      if finElem == it
      then pure fout
      else Left $ FloatingLambdaCannotReturn ot

    _ -> Left $ FloatingLambdaCannotReturn ot

unifySolution (Ast.FloatingLambda (Ast.Body b)) t = do
  actual <- typeOf b
  Left $ UnificationFailure 6 Nothing t actual b

unifySolution _ _ = error "TODO unification error for other cases..."

assertTypeIs :: Type.Type -> Type.Type -> TypeError -> Result ()
assertTypeIs a b err =
  if a == b
  then pure ()
  else Left err

--             lambda        expectedInputType        outputType
unifyLambda :: Ast.Lambda -> Type.Type ->      Result Type.Type
unifyLambda (Ast.Body body) it = do
  ot <- typeOf body
  _  <- unify body ot (Just it)
  pure ot

unifyBinOp :: Ast.Value -> Type.Type -> Ast.Value -> Type.Type -> Env -> Result Env
unifyBinOp a ta b tb env = do
  env' <- unify a ta env
  unify b tb env'

unify :: Ast.Value -> Type.Type -> Maybe Type.Type -> Result Env
unify (Ast.Inte _) Type.Number env = Right env
unify (Ast.Subtract a b) Type.Number env =
  unifyBinOp a Type.Number b Type.Number env

unify (Ast.And a b) Type.Boolean env =
  unifyBinOp a Type.Boolean b Type.Boolean env

unify (Ast.Gt a b) Type.Boolean env =
  unifyBinOp a Type.Number b Type.Number env

unify (Ast.Divide a b) Type.Number env =
  unifyBinOp a Type.Number b Type.Number env

unify ast@(Ast.Identifier name) t env =
  case identType name of
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

unify ast t env = do
  t' <- typeOf ast
  Left $ UnificationFailure 5 env t t' ast

typeOf :: Ast.Value -> Result Type.Type
typeOf (Ast.Inte _)          = Right Type.Number
typeOf (Ast.Gt _ _)          = Right Type.Boolean
typeOf (Ast.And _ _)         = Right Type.Boolean
typeOf (Ast.Divide _ _)      = Right Type.Number
typeOf (Ast.Subtract _ _)    = Right Type.Number
typeOf (Ast.Identifier name) =
  case identType name of
    Nothing -> Left $ IdentifierIsNotDefined name
    Just t  -> Right t

ensureOneFreeOrIdentInEachStep :: Ast.Solution -> Result ()
ensureOneFreeOrIdentInEachStep = go 1 . unpipe
  where

    go n ((Ast.For (Ast.Body l1) (Ast.Body l2) (Ast.Body l3)):rest) =
      oneFreeOrIdent n l1 >> oneFreeOrIdent n l2 >> oneFreeOrIdent n l3 >> go (n+1) rest

    go n ((Ast.FloatingLambda (Ast.Body l)):rest) =
      oneFreeOrIdent n l >> go (n+1) rest

    go _ _ =
      pure ()

    oneFreeOrIdent :: Int -> Ast.Value -> Result ()
    oneFreeOrIdent _ (Ast.Identifier _) = pure ()
    oneFreeOrIdent n ast =
      if S.size (frees ast) /= 1
      then Left $ StepNMustBeIdentiferOrContainSingleFree n ast
      else pure ()

    frees :: Ast.Value -> S.Set Text
    frees (Ast.Gt a b)          = S.union (frees a) (frees b)
    frees (Ast.Divide a b)      = S.union (frees a) (frees b)
    frees (Ast.Subtract a b)    = S.union (frees a) (frees b)
    frees (Ast.And a b)         = S.union (frees a) (frees b)
    frees (Ast.Inte _)          = S.empty
    frees (Ast.Identifier name) =
      case identType name of
        Nothing -> S.singleton name
        Just _ -> S.empty

    unpipe (Ast.Pipe s1 s2) = unpipe s1 ++ unpipe s2
    unpipe v                = [v]

inferInputType :: Ast.Solution -> Result Type.Type
inferInputType s = unifySolution s (Type.List Type.Number)

inferOutputType :: Ast.Solution -> Result Type.Type
inferOutputType (Ast.Pipe _ s)                    = inferOutputType s
inferOutputType (Ast.For _ (Ast.Body l) _)        = inferOutputType' l
inferOutputType (Ast.FloatingLambda (Ast.Body l)) = inferOutputType' l

inferOutputType' :: Ast.Value -> Result Type.Type
inferOutputType' (Ast.Identifier name) =
  case identType name of
    Nothing -> Left $ IdentifierIsNotDefined name
    Just (Type.Arrow _ output) -> pure output
    Just t -> Left $ NotAFunction "output" name t
inferOutputType' (Ast.Gt _ _)       = Right $ Type.List Type.Boolean
inferOutputType' (Ast.And _ _)      = Right $ Type.List Type.Boolean
inferOutputType' (Ast.Divide _ _)   = Right $ Type.List Type.Number
inferOutputType' (Ast.Subtract _ _) = Right $ Type.List Type.Number 
inferOutputType' (Ast.Inte _)       = Left $ NotAFunction "output" "literal" Type.Number
