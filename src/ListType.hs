{-# LANGUAGE OverloadedStrings #-}

module ListType
       ( inferInputType
       ) where

import Builtins (identType)
import Data.Text
import qualified ListAst as Ast
import qualified Type as Type
import qualified Data.Map.Strict as M
import Control.Applicative

data TypeError
  = IdentifierIsNotDefined Text
  | ExpectedAListButGot Type.Type
  | IdentifierNotAFunctionOfAList Text Text Type.Type
  | CouldNotFindExpectedFreeVariable
  deriving Show

type Result a = Either TypeError a

-- TODO: Only uses one variable...

-- for (x > 0) (x / 3 - 2)

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
    seek _ v = typeOfFreeVariable v
