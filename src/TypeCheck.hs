{-# LANGUAGE OverloadedStrings #-}

module TypeCheck
       ( ensureOneFreeOrIdentInEachStep
       , unifySolution
       , unify
       , noFrees
       , TypeError(..)
       ) where

import qualified Ast
import           Data.List (nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import           Data.Text
import qualified Type
import           Value (Context, identType)

type Env = Maybe Type.Type

data TypeError
  = IdentifierIsNotDefined Text
  | FloatingLambdaCannotReturn Int Type.Type
  | IdentifierNotAFunctionOfAList Text Text Type.Type
  | NotAFunction Text Type.Type
  | ValueNotAFunction Type.Type
  | FunctionAppliedToTooManyArgs Type.Type
  | CouldNotInferTypeOfFreeVariableInputIn Ast.Value
  | NoFreesAllowed Ast.Value
  | StepNMustBeIdentiferOrContainSingleFree Int Ast.Value
  | CouldNotInferListType Ast.Value
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
    Type.Arrow Type.Grid arrowOt -> pure arrowOt
    _ -> error "TODO non-unifying grid functions"

unifySolution context (Ast.FloatingLambda lambda) Type.Graph = do
  ot <- unifyLambda context lambda Type.Graph
  case ot of
    Type.Arrow Type.Graph arrowOt -> pure arrowOt
    Type.DijkstraOutputs -> pure Type.DijkstraOutputs
    _ -> error ("TODO non-unifying graph functions " <> show ot)

unifySolution context (Ast.FloatingLambda lambda) Type.DijkstraOutputs = do
  ot <- unifyLambda context lambda Type.DijkstraOutputs
  case ot of
    Type.Arrow Type.DijkstraOutputs arrowOt -> pure arrowOt
    _ -> error ("TODO non-unifying Dijkstra functions " <> show ot)

unifySolution context (Ast.FloatingLambda lambda) Type.Turtle = do
  ot <- unifyLambda context lambda Type.Direction
  case ot of
    Type.Arrow Type.Turtle arrowOt -> pure arrowOt
    v -> error $ "args non-unifying turtle functions " ++ show v

unifySolution context (Ast.FloatingLambda lambda) Type.Program = do
  ot <- unifyLambda context lambda Type.Register
  case ot of
    Type.Arrow Type.Program arrowOt -> pure arrowOt
    _ -> error "TODO non-unifying program functions"

unifySolution context (Ast.FloatingLambda lambda) (Type.List it) = do
  ot <- unifyLambda context lambda it
  case ot of
    Type.Number                         -> pure $ Type.List Type.Number
    Type.Boolean                        -> pure $ Type.List it
    Type.Arrow (Type.List (Type.Var a)) (Type.Var b) ->
      if a == b
      then pure it
      else Left $ FloatingLambdaCannotReturn 1 ot
    Type.Arrow (Type.List (Type.Var a)) (Type.List (Type.Var b)) ->
      if a == b
      then pure $ Type.List it
      else Left $ FloatingLambdaCannotReturn 2 ot
    Type.Arrow (Type.List _) (Type.List b) ->
      pure $ Type.List b
    Type.Arrow Type.Grid Type.Grid ->
      pure Type.Grid
    Type.Arrow Type.Graph Type.Graph ->
      pure Type.Graph
    Type.Arrow Type.Turtle Type.Turtle ->
      pure Type.Turtle
    Type.Arrow (Type.List finElem) fout ->
      if finElem == it || isVar finElem || isVar it
      then pure fout
      else Left $ FloatingLambdaCannotReturn 3 ot

    _ -> error (show (lambda, it))

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

unify context (Ast.Geq a b) Type.Boolean env =
  unifyBinOp context a Type.Number b Type.Number env

unify context (Ast.Add a b) Type.Number env =
  unifyBinOp context a Type.Number b Type.Number env

unify context (Ast.Divide a b) Type.Number env =
  unifyBinOp context a Type.Number b Type.Number env

unify context ast@(Ast.List vs) (Type.List it) env = do
  envs <- mapM (\v -> unify context v it env) vs
  case nub envs of
    [single] -> pure single
    []       -> pure Nothing
    _        -> Left $ CouldNotInferTypeOfFreeVariableInputIn ast

unify context ast@(Ast.Equals a b) Type.Boolean env = do
  t1 <- unify context a (Type.Var 'a') env
  t2 <- unify context b (Type.Var 'a') env
  case (t1, t2) of
    (_, _) | t1 == t2 -> pure t2
    (Just t1', Just t2') -> Left $ UnificationFailure 8 env t1' t2' ast
    _ -> error $ "Unexpected error unifying equals in " ++ show ast ++ " found types " ++ show (t1, t2)

unify context ast@(Ast.Identifier name) t env =
  case identType name context of
    Just t' ->
      if t == t' || isVar t' || isVar t
      then Right (Just t')
      else Left $ UnificationFailure 3 env t t' ast
    Nothing ->
      case env of
        Nothing -> Right (Just t)
        Just t' ->
          if t == t'
          then Right (Just t)
          else
            case (t, t') of
              (Type.List (Type.Var _), Type.List v) ->
                Right $ Just v
              (Type.List v, Type.List (Type.Var _)) ->
                Right $ Just v
              _ -> Left $ UnificationFailure 4 env t' t ast

unify context (Ast.Application fn arguments) t _ =
  case identType fn context of
    Just v ->
      unifyEachArg v $ NE.toList arguments
    Nothing ->
      Left $ IdentifierIsNotDefined fn

  where
    unifyEachArg (Type.Arrow _ ot) (_:args) = do
      if not (ot == t || isVar t)
      -- :(
      then pure $ Just t
      else
        case ot of
          f@(Type.Arrow _ _) -> unifyEachArg f args
          _ -> Right $ Just ot

    -- Mmmm, this is probably not exactly right
    unifyEachArg ot [] = Right $ Just ot

    unifyEachArg ot (_:_) = Left $ ValueNotAFunction ot

unify context ast (Type.Var _) _ = do
  t <- typeOf context ast
  pure $ Just t

unify context ast t env = do
  t' <- typeOf context ast
  if t' == t
  then Right env
  else Left $ UnificationFailure 5 env t t' ast

isVar :: Type.Type -> Bool
isVar (Type.Var _) = True
isVar _ = False

typeOf :: Context -> Ast.Value -> Result Type.Type
typeOf _ (Ast.Inte _)          = Right Type.Number
typeOf _ (Ast.Text _)          = Right Type.Text
typeOf _ (Ast.Gt _ _)          = Right Type.Boolean
typeOf _ (Ast.Geq _ _)         = Right Type.Boolean
typeOf _ (Ast.Lt _ _)          = Right Type.Boolean
typeOf _ (Ast.Leq _ _)         = Right Type.Boolean
typeOf _ (Ast.And _ _)         = Right Type.Boolean
typeOf _ (Ast.Xor _ _)         = Right Type.Boolean
typeOf _ (Ast.Or _ _)          = Right Type.Boolean
typeOf _ (Ast.Equals _ _)      = Right Type.Boolean
typeOf _ (Ast.NotEquals _ _)   = Right Type.Boolean
typeOf _ (Ast.Divide _ _)      = Right Type.Number
typeOf _ (Ast.Multiply _ _)    = Right Type.Number
typeOf _ (Ast.Subtract _ _)    = Right Type.Number
typeOf _ (Ast.Raised _ _)      = Right Type.Number
typeOf _ (Ast.Add _ _)         = Right Type.Number
typeOf _ (Ast.Pos _)           = Right Type.Position

typeOf context ast@(Ast.List vs) = do
  types <- mapM (typeOf context) vs
  case nub types of
    [single] -> pure single
    _        -> Left $ CouldNotInferListType ast

typeOf context (Ast.Identifier name) =
  case identType name context of
    Nothing -> Left $ IdentifierIsNotDefined name
    Just t  -> Right t

typeOf context (Ast.Application name _) =
  case identType name context of
    Just (Type.Arrow _ ot) -> Right ot
    _ -> Left $ IdentifierIsNotDefined name

typeOf context (Ast.FlipCompose f g) = do
  fTy <- inputTypeOf context f
  gTy <- outputTypeOf context g
  pure $ fTy `Type.Arrow` gTy

outputTypeOf :: Context -> Ast.Value -> Result Type.Type
outputTypeOf context v =
  case typeOf context v of
    Right (Type.Arrow _ ot) -> Right $ innermostOutputType ot
    Right t -> Left $ NotAFunction "lvalue" t
    err -> err

  where
    innermostOutputType :: Type.Type -> Type.Type
    innermostOutputType (Type.Arrow _ ot) = innermostOutputType ot
    innermostOutputType ot = ot

inputTypeOf :: Context -> Ast.Value -> Result Type.Type
inputTypeOf context v =
  case typeOf context v of
    Right (Type.Arrow it _) -> Right it
    Right t -> Left $ NotAFunction "lvalue" t
    err -> err

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
    oneFreeOrIdent _ _ (Ast.Application _ _) = pure ()
    oneFreeOrIdent _ _ (Ast.FlipCompose _ _) = pure ()
    oneFreeOrIdent ctx n ast =
      if S.size (frees ctx ast) /= 1
      then Left $ StepNMustBeIdentiferOrContainSingleFree n ast
      else pure ()

    unpipe (Ast.Pipe s1 s2) = unpipe s1 ++ unpipe s2
    unpipe v                = [v]

noFrees :: Context -> Ast.Value -> Result ()
noFrees context ast =
  if S.size (frees context ast) /= 0
  then Left $ NoFreesAllowed ast
  else pure ()

frees :: Context -> Ast.Value -> S.Set Text
frees context (Ast.Gt a b)          = S.union (frees context a) (frees context b)
frees context (Ast.Geq a b)         = S.union (frees context a) (frees context b)
frees context (Ast.Leq a b)         = S.union (frees context a) (frees context b)
frees context (Ast.Lt a b)          = S.union (frees context a) (frees context b)
frees context (Ast.Divide a b)      = S.union (frees context a) (frees context b)
frees context (Ast.Multiply a b)    = S.union (frees context a) (frees context b)
frees context (Ast.Subtract a b)    = S.union (frees context a) (frees context b)
frees context (Ast.Add a b)         = S.union (frees context a) (frees context b)
frees context (Ast.Xor a b)         = S.union (frees context a) (frees context b)
frees context (Ast.Raised a b)      = S.union (frees context a) (frees context b)
frees context (Ast.And a b)         = S.union (frees context a) (frees context b)
frees context (Ast.Or a b)          = S.union (frees context a) (frees context b)
frees context (Ast.Equals a b)      = S.union (frees context a) (frees context b)
frees context (Ast.NotEquals a b)   = S.union (frees context a) (frees context b)
frees context (Ast.List vs)         = S.unions $ frees context <$> vs
frees _       (Ast.Inte _)          = S.empty
frees _       (Ast.Pos _)           = S.empty
frees _       (Ast.Text _)          = S.empty
frees context (Ast.FlipCompose f g) = S.union (frees context f) (frees context g)
frees context (Ast.Identifier name) =
  case identType name context of
    Nothing -> S.singleton name
    Just _ -> S.empty

frees context (Ast.Application fn args) =
  S.union (frees context (Ast.Identifier fn)) (S.unions $ NE.toList $ (frees context <$> args))
