module ListEvaluator
       ( EvalError(..)
       , eval
       ) where

import qualified ListAst as Ast
import qualified Value as Value
import qualified Builtins as Env
import qualified Data.Text as Text
import Debug.Trace

data EvalError
  = UnexpectedError Int
  | TypeMismatchAtRuntime Text.Text
  deriving Show

type Result a = Either EvalError a

eval :: Value.Value -> Ast.Solution -> Result Value.Value
eval v (Ast.Pipe s1 s2) = do
  v' <- eval v s1
  eval v' s2

eval (Value.Vs vs) (Ast.For cond gen reduce) = do
  results <- sequence $ aux <$> vs
  pure $ Value.Vs $ concat results

  where
    aux :: Value.Value -> Result [Value.Value]
    aux v = do
      result  <- applyLambda v gen
      reduced <- applyLambda v reduce
      condMet <- applyLambda reduced cond
      case condMet of
        Value.True -> do
          rest <- aux reduced
          pure $ result:rest

        Value.False -> pure []
        bad         -> Left $ TypeMismatchAtRuntime (Text.pack ("When applying condition in `for` got " ++ show bad ++ " but expected a boolean value"))

eval (Value.Vs vs) (Ast.FloatingLambda (Ast.Body (Ast.Identifier name))) =
  case Env.identValue name of
    Just (Value.Fold (initial, step)) ->
      case foldr (\v acc -> acc >>= step v) (Just initial) vs of
        Nothing -> Left $ UnexpectedError 1
        Just v  -> Right v
    Just (Value.StepsOfFold (initial, step)) ->
      Value.Vs <$> foldSteps step initial vs

    Just (Value.Func f) ->
      case f (Value.Vs vs) of
        Nothing -> Left $ UnexpectedError 2
        Just v  -> Right v

    Just _ -> Left $ TypeMismatchAtRuntime (Text.pack ("Built-in " ++ Text.unpack name ++ " specified at the top level of list evaluation but it's not a list function" ))
    Nothing -> Right $ Value.Vs vs

  where
    apply step v acc =
      case step v acc of
        Just v -> Right v
        Nothing -> Left $ UnexpectedError 3

    foldSteps step acc [] = Right []
    foldSteps step acc (v:vs) = do
      result <- apply step v acc
      results <- foldSteps step result vs
      pure $ result:results

eval (Value.Vs vs) (Ast.FloatingLambda lambda) = do
  results <- sequence $ applyAndKeepOriginal <$> vs
  pure $ Value.Vs $ results >>= pick

    where
      pick (v, Value.True) = [v]
      pick (v, Value.False) = []
      pick (_, v) = [v]

      applyAndKeepOriginal v = do
        result <- applyLambda v lambda
        pure (v, result)

eval v (Ast.FloatingLambda (Ast.Body op)) = Left $ TypeMismatchAtRuntime (Text.pack ("When applying top level operation " ++ show op ++ " got " ++ show v ++ " but expected a list"))

eval v (Ast.For _ _ _) = Left $ TypeMismatchAtRuntime (Text.pack ("When applying `for` got " ++ show v ++ " but expected a list"))

applyLambda :: Value.Value -> Ast.Lambda -> Result Value.Value
applyLambda v (Ast.Body lambda) = evalValue (Just v) lambda

--            value for free var
evalValue :: Maybe Value.Value -> Ast.Value -> Result Value.Value
evalValue val (Ast.Identifier name) =
  case Env.identValue name of
    Just v -> Right v
    Nothing ->
      case val of
        Nothing -> Left $ UnexpectedError 4
        Just v  -> Right v

evalValue val (Ast.Gt a b) =
  binNumberOp val (>) ">" a b toBoolean
  
evalValue val (Ast.Divide a b) =
  binNumberOp val div "/" a b Value.I

evalValue val (Ast.Subtract a b) =
  binNumberOp val (-) "-" a b Value.I

evalValue val (Ast.And a b) =
  binBooleanOp val (&&) "&&" a b toBoolean

evalValue _ (Ast.Inte v) = Right $ Value.I v

toBoolean True = Value.True
toBoolean False = Value.False

fromBoolean Value.True = Just True
fromBoolean Value.False = Just False
fromBoolean _ = Nothing

binNumberOp val op opName a b toValue =  do
  a' <- evalValue val a
  b' <- evalValue val b
  case (a', b') of
    (Value.I a'', Value.I b'') -> Right $ toValue $ op a'' b''
    _                          -> Left $ TypeMismatchAtRuntime (Text.pack ("When applying `" ++ opName ++ "` got " ++ show (a', b') ++ " but expected numbers"))

binBooleanOp val op opName a b toValue = do
  a' <- evalValue val a
  b' <- evalValue val b
  case (fromBoolean a', fromBoolean b') of
    (Just a'', Just b'') -> Right $ toValue $ op a'' b''
    _                -> Left $ TypeMismatchAtRuntime (Text.pack ("When applying `" ++ opName ++ "` got " ++ show (a', b') ++ " but expected booleans"))

