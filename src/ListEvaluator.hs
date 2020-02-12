module ListEvaluator
       ( EvalError(..)
       , eval
       ) where

import qualified Ast as Ast
import           Builtins (Context, identValue)
import qualified Data.Text as Text
import qualified Value as Value

data EvalError
  = UnexpectedError Int
  | TypeMismatchAtRuntime Text.Text
  deriving Show

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
      case f (Value.Vs vs) of
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

eval _ v (Ast.FloatingLambda (Ast.Body op)) = Left $ TypeMismatchAtRuntime (Text.pack ("When applying top level operation " ++ show op ++ " got " ++ show v ++ " but expected a list"))

eval _ v (Ast.For _ _ _) = Left $ TypeMismatchAtRuntime (Text.pack ("When applying `for` got " ++ show v ++ " but expected a list"))

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

evalValue _ _ (Ast.Application _ _) =
  error "TODO: make application work"

evalValue context val (Ast.Gt a b) =
  binNumberOp context val (>) ">" a b toBoolean

evalValue context val (Ast.Divide a b) =
  binNumberOp context val div "/" a b Value.I

evalValue context val (Ast.Subtract a b) =
  binNumberOp context val (-) "-" a b Value.I

evalValue context val (Ast.Add a b) =
  binNumberOp context val (+) "+" a b Value.I

evalValue context val (Ast.Raised a b) =
  binNumberOp context val (^) "^" a b Value.I

evalValue context val (Ast.And a b) =
  binBooleanOp context val (&&) "&&" a b toBoolean

evalValue context val (Ast.Or a b) =
  binBooleanOp context val (||) "||" a b toBoolean

evalValue context val (Ast.Equals a b) =
  binBooleanOp context val (==) "=" a b toBoolean

evalValue _ _ (Ast.Inte v) = Right $ Value.I v

toBoolean :: Bool -> Value.Value
toBoolean True = Value.True
toBoolean False = Value.False

fromBoolean :: Value.Value -> Maybe Bool
fromBoolean Value.True = Just True
fromBoolean Value.False = Just False
fromBoolean _ = Nothing

binNumberOp :: Context -> (Maybe Value.Value) -> (Integer -> Integer -> t) -> String -> Ast.Value -> Ast.Value -> (t -> b) -> Result b
binNumberOp context val op opName a b toValue =  do
  a' <- evalValue context val a
  b' <- evalValue context val b
  case (a', b') of
    (Value.I a'', Value.I b'') -> Right $ toValue $ op a'' b''
    _                          -> Left $ TypeMismatchAtRuntime (Text.pack ("When applying `" ++ opName ++ "` got " ++ show (a', b') ++ " but expected numbers"))

binBooleanOp :: Context -> (Maybe Value.Value) -> (Bool -> Bool -> t) -> String -> Ast.Value -> Ast.Value -> (t -> b) -> Result b
binBooleanOp context val op opName a b toValue = do
  a' <- evalValue context val a
  b' <- evalValue context val b
  case (fromBoolean a', fromBoolean b') of
    (Just a'', Just b'') -> Right $ toValue $ op a'' b''
    _                -> Left $ TypeMismatchAtRuntime (Text.pack ("When applying `" ++ opName ++ "` got " ++ show (a', b') ++ " but expected booleans"))

