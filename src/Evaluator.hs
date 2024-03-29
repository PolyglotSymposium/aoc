module Evaluator
       ( EvalError(..)
       , eval
       , evalValue
       , toBoolean
       ) where

import qualified Ast
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Bits as Bits
import qualified Value
import           Value (Context, identValue)

data EvalError
  = UnexpectedError Int
  | TypeMismatchAtRuntime Text.Text
  deriving (Show, Eq)

type Result a = Either EvalError a

eval :: Context -> Value.Value -> Ast.Solution -> Result Value.Value
eval context v (Ast.Pipe s1 s2) = do
  v' <- eval context v s1
  eval context v' s2

eval context (Value.Vs vs) (Ast.For cond gen reduce) = do
  results <- sequence $ aux <$> vs
  pure $ Value.Vs $ concat results

  where
    aux :: Value.Value -> Result [Value.Value]
    aux v = do
      result  <- applyLambda context v gen
      reduced <- applyLambda context v reduce
      condMet <- applyLambda context reduced cond
      case condMet of
        Value.True -> do
          rest <- aux reduced
          pure $ result:rest

        Value.False -> pure []
        bad         -> Left $ TypeMismatchAtRuntime (Text.pack ("When applying condition in `for` got " ++ show bad ++ " but expected a boolean value"))

eval context (Value.Vs vs) (Ast.FloatingLambda (Ast.Body (Ast.Identifier name))) =
  case identValue name context of
    Just (Value.Fold (initial, step)) ->
      case foldr (\v acc -> acc >>= step v) (Just initial) vs of
        Nothing -> Left $ UnexpectedError 1
        Just v  -> Right v
    Just (Value.StepsOfFold (initial, step)) ->
      Value.Vs <$> foldSteps step initial vs

    Just (Value.Func f) ->
      case f context (Value.Vs vs) of
        Nothing -> Left $ UnexpectedError 2
        Just v  -> Right v

    Just _ -> Left $ TypeMismatchAtRuntime (Text.pack ("Built-in " ++ Text.unpack name ++ " specified at the top level of list evaluation but it's not a list function" ))
    Nothing -> Right $ Value.Vs vs

  where
    apply step v acc =
      case step v acc of
        Just v' -> Right v'
        Nothing -> Left $ UnexpectedError 3

    foldSteps _ _ [] = Right []
    foldSteps step acc (v:vs') = do
      result <- apply step v acc
      results <- foldSteps step result vs'
      pure $ result:results

eval context freeArg (Ast.FloatingLambda (Ast.Body (Ast.Application fn args))) = do
  fnValue <- evalValue context Nothing (Ast.Identifier fn)
  case fnValue of
    Value.Func f -> do
      v <- applyAll (Value.Func f) $ NE.toList args
      case v of
        Value.Func g ->
          case g context freeArg of
            Nothing -> Left $ UnexpectedError 9
            Just v' -> Right v'
        v' -> pure v'
    Value.Fold (initial, step) ->
      case args of
        onlyArg NE.:| [] -> do
          argValue <- evalValue context (Just freeArg) onlyArg
          case argValue of
            Value.Vs vs ->
              Value.Vs <$> traverse (applyFold initial step) vs
            _ -> Left $ UnexpectedError 89233289
        _ ->
          Left $ UnexpectedError 8932382

    _ -> Left $ UnexpectedError 7

    where
      applyAll (Value.Func f) (unappliedArg:unappliedArgs) = do
        argValue <- evalValue context (Just freeArg) unappliedArg
        case f context argValue of
          Just f' -> applyAll f' unappliedArgs
          _ -> error (Text.unpack fn)
      applyAll v [] = pure v
      applyAll _ _ = Left $ UnexpectedError 5423

      applyFold initial step (Value.Vs vs) =
        case foldr (\v acc -> acc >>= step v) (Just initial) vs of
          Nothing -> Left $ UnexpectedError 1
          Just v  -> Right v
      applyFold _ _ _ = Left $ UnexpectedError 323432

eval context val (Ast.FloatingLambda (Ast.Body fc@(Ast.FlipCompose _ _))) =
  evalValue context (Just val) fc

eval context (Value.Vs vs) (Ast.FloatingLambda lambda) = do
  results <- sequence $ applyAndKeepOriginal <$> vs
  pure $ Value.Vs $ results >>= pick

    where
      pick (v, Value.True) = [v]
      pick (_, Value.False) = []
      pick (_, v) = [v]

      applyAndKeepOriginal v = do
        result <- applyLambda context v lambda
        pure (v, result)

eval context grid@Value.Turtle{} (Ast.FloatingLambda (Ast.Body (Ast.Identifier name))) =
  case identValue name context of
    Just (Value.Func f) ->
      case f context grid of
        Nothing -> Left $ UnexpectedError 32
        Just v  -> Right v

    _ -> Left $ TypeMismatchAtRuntime (Text.pack ("Built-in " ++ Text.unpack name ++ " specified at the top level of turtle evaluation but it's not a turtle function" ))

eval context grid@Value.Grid{} (Ast.FloatingLambda (Ast.Body (Ast.Identifier name))) =
  case identValue name context of
    Just (Value.Func f) ->
      case f context grid of
        Nothing -> Left $ UnexpectedError 22
        Just v  -> Right v

    _ -> Left $ TypeMismatchAtRuntime (Text.pack ("Built-in " ++ Text.unpack name ++ " specified at the top level of conway evaluation but it's not a conway function" ))

eval context graph@Value.Graph{} (Ast.FloatingLambda (Ast.Body (Ast.Identifier name))) =
  case identValue name context of
    Just (Value.Func f) ->
      case f context graph of
        Nothing -> Left $ UnexpectedError 25
        Just v  -> Right v

    _ -> Left $ TypeMismatchAtRuntime (Text.pack ("Built-in " ++ Text.unpack name ++ " specified at the top level of graph evaluation but it's not a graph function" ))

eval context dos@(Value.DijkstraOutputs _) (Ast.FloatingLambda (Ast.Body (Ast.Identifier name))) =
  case identValue name context of
    Just (Value.Func f) ->
      case f context dos of
        Nothing -> Left $ UnexpectedError 24
        Just v  -> Right v

    _ -> Left $ TypeMismatchAtRuntime (Text.pack ("Built-in " ++ Text.unpack name ++ " specified at the top level of conway evaluation but it's not a conway function" ))

eval context program@Value.Program{} (Ast.FloatingLambda (Ast.Body (Ast.Identifier name))) =
  case identValue name context of
    Just (Value.Func f) ->
      case f context program of
        Nothing -> Left $ UnexpectedError 23
        Just v  -> Right v

    _ -> Left $ TypeMismatchAtRuntime (Text.pack ("Built-in " ++ Text.unpack name ++ " specified at the top level of program evaluation but it's not a program function" ))

eval _ v (Ast.FloatingLambda (Ast.Body op)) = Left $ TypeMismatchAtRuntime (Text.pack ("When applying top level operation " ++ show op ++ " got " ++ show v ++ " which is not expected as a top-level input"))

eval _ v Ast.For{} = Left $ TypeMismatchAtRuntime (Text.pack ("When applying `for` got " ++ show v ++ " but expected a list"))

applyLambda :: Context -> Value.Value -> Ast.Lambda -> Result Value.Value
applyLambda context v (Ast.Body lambda) = evalValue context (Just v) lambda

--                      value for free var
evalValue :: Context -> Maybe Value.Value -> Ast.Value -> Result Value.Value
evalValue context val (Ast.Identifier name) =
  case identValue name context of
    Just v -> Right v
    Nothing ->
      case val of
        Nothing -> Left $ UnexpectedError 4
        Just v  -> Right v

evalValue context val (Ast.Application fn args@(arg NE.:| _)) = do
  fnValue <- evalValue context val (Ast.Identifier fn)
  argValue <- evalValue context val arg
  case (fnValue, argValue) of
    (Value.Func f, _) ->
      applyAll (Value.Func f) $ NE.toList args

    (Value.StepsOfFold (initial, step), Value.Vs vs) ->
      Value.Vs <$> foldSteps step initial vs

    (Value.Fold (initial, step), Value.Vs vs) ->
      case foldr (\v acc -> acc >>= step v) (Just initial) vs of
        Nothing -> Left $ UnexpectedError 1
        Just v  -> Right v

    _ -> Left $ UnexpectedError 66

  where
    applyAll (Value.Func f) (unappliedArg:unappliedArgs) = do
      argValue <- evalValue context val unappliedArg
      case f context argValue of
        Just f' -> applyAll f' unappliedArgs
        _ -> error (Text.unpack fn <> " " <> show argValue)

    applyAll v [] = pure v
    applyAll _ _ = Left $ UnexpectedError 5423

    apply step v acc =
      case step v acc of
        Just v' -> Right v'
        Nothing -> Left $ UnexpectedError 3

    foldSteps _ _ [] = Right []
    foldSteps step acc (v:vs') = do
      result <- apply step v acc
      results <- foldSteps step result vs'
      pure $ result:results

evalValue context val (Ast.Gt a b) =
  binNumberOp context val (>) ">" a b toBoolean

evalValue context val (Ast.Geq a b) =
  binNumberOp context val (>=) ">=" a b toBoolean

evalValue context val (Ast.Lt a b) =
  binNumberOp context val (<) "<" a b toBoolean

evalValue context val (Ast.Leq a b) =
  binNumberOp context val (<=) "<=" a b toBoolean

evalValue context val (Ast.Divide a b) =
  binNumberOp context val div "/" a b Value.I

evalValue context val (Ast.Multiply a b) =
  binNumberOp context val (*) "*" a b Value.I

evalValue context val (Ast.Subtract a b) =
  binNumberOp context val (-) "-" a b Value.I

evalValue context val (Ast.Add a b) =
  binNumberOp context val (+) "+" a b Value.I

evalValue context val (Ast.Raised a b) =
  binNumberOp context val (^) "^" a b Value.I

evalValue context val (Ast.And a b) =
  binBooleanOp context val (&&) "&&" a b toBoolean

evalValue context val (Ast.Xor a b) =
  binBooleanOp context val Bits.xor ".^." a b toBoolean

evalValue context val (Ast.Or a b) =
  binBooleanOp context val (||) "||" a b toBoolean

evalValue context val (Ast.Equals a b) =
  checkEqual context val a b

evalValue context val (Ast.NotEquals a b) =
  binNumberOp context val (/=) "/=" a b toBoolean

evalValue context val (Ast.List vs) = do
  vs' <- sequence $ evalValue context val <$> vs
  pure $ Value.Vs vs'

evalValue _ _ (Ast.Inte v) = Right $ Value.I v

evalValue _ _ (Ast.Pos pos) = Right $ Value.Pos pos

evalValue _ _ (Ast.Text t) = Right $ Value.Txt t

evalValue context val (Ast.FlipCompose f g) =
  case (val, evalValue context Nothing f, evalValue context Nothing g) of
    (Just val', Right (Value.Func f'), Right (Value.Func g')) ->
      case f' context val' of
        Nothing -> Left $ UnexpectedError 72
        Just v  ->
          case g' context v of
            Nothing -> Left $ UnexpectedError 73
            Just vOut -> Right vOut
    _ -> Left $ UnexpectedError 99

toBoolean :: Bool -> Value.Value
toBoolean True = Value.True
toBoolean False = Value.False

fromBoolean :: Value.Value -> Maybe Bool
fromBoolean Value.True = Just True
fromBoolean Value.False = Just False
fromBoolean _ = Nothing

binNumberOp :: Context -> Maybe Value.Value -> (Integer -> Integer -> t) -> String -> Ast.Value -> Ast.Value -> (t -> b) -> Result b
binNumberOp context val op opName a b toValue = do
  a' <- evalValue context val a
  b' <- evalValue context val b
  case (a', b') of
    (Value.I a'', Value.I b'') -> Right $ toValue $ op a'' b''
    _                          -> Left $ TypeMismatchAtRuntime (Text.pack ("When applying `" ++ opName ++ "` got " ++ show (a', b') ++ " but expected numbers"))

binBooleanOp :: Context -> Maybe Value.Value -> (Bool -> Bool -> t) -> String -> Ast.Value -> Ast.Value -> (t -> b) -> Result b
binBooleanOp context val op opName a b toValue = do
  a' <- evalValue context val a
  b' <- evalValue context val b
  case (fromBoolean a', fromBoolean b') of
    (Just a'', Just b'') -> Right $ toValue $ op a'' b''
    _                -> Left $ TypeMismatchAtRuntime (Text.pack ("When applying `" ++ opName ++ "` got " ++ show (a', b') ++ " but expected booleans"))


checkEqual :: Context -> Maybe Value.Value -> Ast.Value -> Ast.Value -> Result Value.Value
checkEqual context val a b = do
  a' <- evalValue context val a
  b' <- evalValue context val b
  case (a', b') of
    (Value.I a'', Value.I b'') -> Right $ toBoolean $ a'' == b''
    (Value.Ch a'', Value.Ch b'') -> Right $ toBoolean $ a'' == b''
    (Value.Txt a'', Value.Txt b'') -> Right $ toBoolean $ a'' == b''
    _                          -> Left $ TypeMismatchAtRuntime (Text.pack ("When applying `=` got " ++ show (a', b') ++ " but expected matching types"))
