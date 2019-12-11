module ListEvaluator
       ( EvalError(..)
       , eval
       ) where

import qualified ListAst as Ast
import qualified Value as Value
import qualified Builtins as Env

data EvalError = UnexpectedError

type Result a = Either EvalError a

eval :: Value.Value -> Ast.Solution -> Result Value.Value
eval v (Ast.Pipe s1 s2) = do
  v' <- eval v s1
  eval v' s2

-- for (x > 0) (x / 3 - 2)
eval (Value.Vs vs) (Ast.For cond (Ast.Body gen) (Ast.Body reduce)) =
  let a = map aux vs
  in
    undefined

  where
    aux :: Value.Value -> Result [Value.Value]
    aux v = do
      applyLambda v cond 
      undefined

eval v (Ast.FloatingLambda lambda) = undefined

eval _ _ = Left UnexpectedError

applyLambda :: Value.Value -> Ast.Lambda -> Result Value.Value
applyLambda v (Ast.Body lambda) = evalValue (Just v) lambda

--            value for free var
evalValue :: Maybe Value.Value -> Ast.Value -> Result Value.Value
evalValue val (Ast.Identifier name) =
  case Env.identValue name of
    Just v -> Right v
    Nothing ->
      case val of
        Nothing -> Left UnexpectedError
        Just v  -> Right v
