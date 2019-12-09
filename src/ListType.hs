{-# LANGUAGE OverloadedStrings #-}

module ListType
       ( inferInputType
       , ensureOneFreeOrIdentInEachStep
       ) where

import Builtins (identType)
import Data.Text
import qualified ListAst as Ast
import qualified Type as Type
import qualified Data.Set as S
import Control.Applicative

data TypeError
  = IdentifierIsNotDefined Text
  | ExpectedAListButGot Type.Type
  | IdentifierNotAFunctionOfAList Text Text Type.Type
  | CouldNotFindExpectedFreeVariable
  | StepNMustBeIdentiferOrContainSingleFree Int Ast.Value
  deriving Show

type Result a = Either TypeError a

ensureOneFreeOrIdentInEachStep :: Ast.Solution -> Result ()
ensureOneFreeOrIdentInEachStep = go 1 . unpipe
  where

    go n ((Ast.For (Ast.Body l1) (Ast.Body l2) Nothing):rest) =
      oneFreeOrIdent n l1 >> oneFreeOrIdent n l2 >> go (n+1) rest

    go n ((Ast.For (Ast.Body l1) (Ast.Body l2) (Just (Ast.Body l3))):rest) =
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
    Nothing -> Left CouldNotFindExpectedFreeVariable

typeOfFreeVariable :: Ast.Value -> Maybe Type.Type
typeOfFreeVariable v =
  case v of
    Ast.Gt a b           -> seek2 Type.Number a b
    Ast.Divide a b       -> seek2 Type.Number a b
    Ast.Subtract a b     -> seek2 Type.Number a b
    Ast.Inte _           -> Nothing
    Ast.Identifier name ->
      case identType name of
        Nothing -> pure Type.Variable
        Just _ -> Nothing

  where
    seek2 t a b = seek t a <|> seek t b

    seek t (Ast.Identifier name) =
      case identType name of
        Nothing -> pure t
        Just _ -> Nothing
    seek _ v' = typeOfFreeVariable v'
