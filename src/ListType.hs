{-# LANGUAGE OverloadedStrings #-}

module ListType
       ( inferInputType
       , inferOutputType
       , ensureOneFreeOrIdentInEachStep
       , check
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
  | NotAFunction Text Text Type.Type
  | CouldNotFindExpectedFreeVariableIn Ast.Value
  | StepNMustBeIdentiferOrContainSingleFree Int Ast.Value
  | SolutionTypeCheckFailed Ast.Solution Type.Type Type.Type 
  | TypeCheckFailed Ast.Value Type.Type Type.Type 
  | NotBinaryFunction Ast.Value
  deriving Show

type Result a = Either TypeError a

check :: Ast.Solution -> Type.Type -> Result Type.Type
check (Ast.Pipe s1 s2) (Type.Arrow i o) = do
  let s1Expected = Type.Arrow i (Type.Var 'a')
  s1Actual <- check s1 s1Expected
  case s1Actual of
    (Type.Arrow s1In s1Out) -> do
      s2Actual <- check s2 (Type.Arrow s1Out o)
      case s2Actual of
        (Type.Arrow _ s2Out) -> pure $ Type.Arrow s1In s2Out

    _ -> Left $ SolutionTypeCheckFailed s1 s1Actual s1Expected

check (Ast.For cond gen reduce) (Type.Arrow (Type.List i) (Type.List o)) = do
  _         <- checkVal (Ast.body cond) (Type.Arrow i Type.Boolean)
  let genExpected = Type.Arrow i (Type.Var 'a')
  genActual <- checkVal (Ast.body gen) genExpected
  case genActual of
    (Type.Arrow i o) -> pure $ Type.Arrow (Type.List i) (Type.List o)
    _                -> Left $ TypeCheckFailed (Ast.body gen) genActual genExpected

check (Ast.FloatingLambda lambda) t = checkLambda lambda t
check s (Type.Var _) = typeOf s
check s t = do
  actual <- typeOf s
  if actual == t
  then Right actual
  else Left $ SolutionTypeCheckFailed s actual t

typeOf :: Ast.Solution -> Result Type.Type
typeOf _ = undefined

args :: Ast.Value -> Result (Ast.Value, Ast.Value)
args (Ast.Gt a b) = pure (a, b)
args (Ast.Subtract a b) = pure (a, b)
args (Ast.Divide a b) = pure (a, b)
args ast = Left $ NotBinaryFunction ast

checkBin :: Ast.Value -> Type.Type -> Type.Type -> Type.Type -> Result Type.Type
checkBin ast argType input output = do
  (a, b) <- args ast
  _ <- checkVal a argType
  _ <- checkVal b argType
  case typeOfFreeVariable ast of
    Just t | t == input -> Right $ Type.Arrow (Type.List input) (Type.List output)
    Nothing -> Left $ CouldNotFindExpectedFreeVariableIn ast

checkLambda :: Ast.Lambda -> Type.Type -> Result Type.Type
checkLambda (Ast.Body ast@(Ast.Gt _ _)) (Type.Arrow (Type.List input) (Type.List output)) = do
  checkBin ast Type.Number input output

checkLambda (Ast.Body ast@(Ast.Divide a b)) (Type.Arrow (Type.List input) (Type.List output)) = do
  checkBin ast Type.Number input Type.Number

checkLambda (Ast.Body ast@(Ast.Subtract a b)) (Type.Arrow (Type.List input) (Type.List output)) = do
  checkBin ast Type.Number input Type.Number

checkVal :: Ast.Value -> Type.Type -> Result Type.Type
checkVal = undefined

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

typeOfFreeVariable :: Ast.Value -> Maybe Type.Type
typeOfFreeVariable v =
  case v of
    Ast.Gt a b           -> seek2 Type.Number a b
    Ast.Divide a b       -> seek2 Type.Number a b
    Ast.Subtract a b     -> seek2 Type.Number a b
    Ast.Inte _           -> Nothing
    Ast.Identifier name ->
      case identType name of
        Nothing -> pure $ Type.Var 'a'
        Just _ -> Nothing

  where
    seek2 t a b = seek t a <|> seek t b

    seek t (Ast.Identifier name) =
      case identType name of
        Nothing -> pure t
        Just _ -> Nothing
    seek _ v' = typeOfFreeVariable v'
