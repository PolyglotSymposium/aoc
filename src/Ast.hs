module Ast
  ( Solution(..)
  , Lambda(..)
  , Value(..)
  , substitute
  ) where

import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Text

data Solution
  = Pipe Solution Solution
  | For Lambda Lambda Lambda
  | FloatingLambda Lambda
  deriving (Show, Eq)

newtype Lambda
  = Body { body::Value }
  deriving (Show, Eq)

data Value
  = Gt Value Value
  | Geq Value Value
  | Leq Value Value
  | Lt Value Value
  | And Value Value
  | Or Value Value
  | Divide Value Value
  | Multiply Value Value
  | Add Value Value
  | Subtract Value Value
  | Raised Value Value
  | Equals Value Value
  | NotEquals Value Value
  | Identifier Text
  | Inte Integer
  | Application Text Value
  | List [Value]
  | Pos (Integer, Integer)
  | FlipCompose Value Value
  deriving (Show, Eq)

substitute :: M.Map Text Value -> Value -> Value
substitute subs (Gt l r)             = Gt        (substitute subs l) (substitute subs r)
substitute subs (Geq l r)            = Geq       (substitute subs l) (substitute subs r)
substitute subs (Leq l r)            = Leq       (substitute subs l) (substitute subs r)
substitute subs (Lt l r)             = Lt        (substitute subs l) (substitute subs r)
substitute subs (And l r)            = And       (substitute subs l) (substitute subs r)
substitute subs (Or l r)             = Or        (substitute subs l) (substitute subs r)
substitute subs (Divide l r)         = Divide    (substitute subs l) (substitute subs r)
substitute subs (Multiply l r)       = Multiply  (substitute subs l) (substitute subs r)
substitute subs (Add l r)            = Add       (substitute subs l) (substitute subs r)
substitute subs (Subtract l r)       = Subtract  (substitute subs l) (substitute subs r)
substitute subs (Raised l r)         = Raised    (substitute subs l) (substitute subs r)
substitute subs (Equals l r)         = Equals    (substitute subs l) (substitute subs r)
substitute subs (NotEquals l r)      = NotEquals (substitute subs l) (substitute subs r)
substitute _ num@(Inte _)            = num
substitute _ pos@(Pos _)             = pos
substitute subs (Application fn arg) = Application fn $ substitute subs arg
substitute subs (List vs)            = List $ substitute subs <$> vs
substitute subs (FlipCompose f g)    = FlipCompose (substitute subs f) $ substitute subs g
substitute subs (Identifier name) =
  fromMaybe (Identifier name) $ M.lookup name subs
