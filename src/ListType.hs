{-# LANGUAGE OverloadedStrings #-}

module ListType
       ( inferInputType
       , inferOutputType
       , ensureOneFreeOrIdentInEachStep
       , unify
       , TypeError(..)
       ) where

import Builtins (identType)
import Data.Text
import qualified ListAst as Ast
import qualified Type as Type
import qualified Data.Set as S
import Control.Applicative

type Env = Maybe Type.Type

data TypeError
  = IdentifierIsNotDefined Text
  | ExpectedAListButGot Type.Type
  | IdentifierNotAFunctionOfAList Text Text Type.Type
  | NotAFunction Text Text Type.Type
  | CouldNotFindExpectedFreeVariableIn Ast.Value
  | StepNMustBeIdentiferOrContainSingleFree Int Ast.Value
  | SolutionTypeCheckFailed Ast.Solution Type.Type Type.Type 
  | TypeCheckFailed Ast.Value Type.Type Type.Type 
  | NotBinaryFunction Ast.Value
  --                   resolved expectedType actualType value
  | UnificationFailure Env      Type.Type    Type.Type  Ast.Value
  deriving (Show, Eq)

type Result a = Either TypeError a

unify :: Ast.Value -> Type.Type -> Maybe Type.Type -> Result Env
unify (Ast.Inte _) Type.Number env = Right env
unify (Ast.Subtract a b) Type.Number env = do
  env' <- unify a Type.Number env
  unify b Type.Number env'
unify (Ast.And a b) Type.Boolean env = do
  env' <- unify a Type.Boolean env
  unify b Type.Boolean env'
unify (Ast.Gt a b) Type.Boolean env = do
  env' <- unify a Type.Number env
  unify b Type.Number env'

unify ast@(Ast.Identifier name) t env =
  case identType name of
    Just t' ->
      if t == t'
      then Right env
      else Left $ UnificationFailure env t t' ast
    Nothing ->
      case env of
        Nothing -> Right (Just t)
        Just t' ->
          if t == t'
          then Right (Just t)
          else Left $ UnificationFailure env t' t ast

typeOf :: Ast.Value -> Result Type.Type
typeOf (Ast.Inte _) = Right Type.Number

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
inferInputType (Ast.Pipe s _)                    = inferInputType s
inferInputType (Ast.For _ (Ast.Body l) _)        = inferInputType' l
inferInputType (Ast.FloatingLambda (Ast.Body l)) = inferInputType' l

inferInputType' :: Ast.Value -> Result Type.Type
inferInputType' (Ast.Identifier name) =
  case identType name of
    Nothing -> Left $ IdentifierIsNotDefined name
    Just (Type.Arrow (Type.List i) _) -> pure i
    Just t -> Left $ IdentifierNotAFunctionOfAList "input" name t
inferInputType' v =
  case typeOfFreeVariable v of
    Just t -> pure t
    Nothing -> Left $ CouldNotFindExpectedFreeVariableIn v

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
inferOutputType' (Ast.Divide _ _)   = Right $ Type.List Type.Number
inferOutputType' (Ast.Subtract _ _) = Right $ Type.List Type.Number 
inferOutputType' (Ast.Inte _)       = Left $ NotAFunction "output" "literal" Type.Number

-- TODO: Unify should work here once implemented
typeOfFreeVariable :: Ast.Value -> Maybe Type.Type
typeOfFreeVariable v =
  case v of
    Ast.Gt a b           -> seek2 Type.Number a b
    Ast.Divide a b       -> seek2 Type.Number a b
    Ast.Subtract a b     -> seek2 Type.Number a b
    Ast.Inte _           -> Nothing
    Ast.Identifier name ->
      case identType name of
        Nothing -> pure $ error "TODO"
        Just _ -> Nothing

  where
    seek2 t a b = seek t a <|> seek t b

    seek t (Ast.Identifier name) =
      case identType name of
        Nothing -> pure t
        Just _ -> Nothing
    seek _ v' = typeOfFreeVariable v'
