module Ast
  ( Solution(..)
  , Lambda(..)
  , Value(..)
  ) where

import Data.Text

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
  | And Value Value
  | Or Value Value
  | Divide Value Value
  | Add Value Value
  | Subtract Value Value
  | Raised Value Value
  | Equals Value Value
  | Identifier Text
  | Inte Integer
  | Application Text Value
  deriving (Show, Eq)
