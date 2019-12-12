module ListAst
       ( Problem(..)
       , Solution(..)
       , Lambda(..)
       , Value(..)
       ) where

import Data.Text

data Problem =
  ListProblem
  { at :: Text
  , separator :: Text
  , solution :: Solution
  , repeatsForever :: Bool
  } deriving (Show, Eq)

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
  | And Value Value
  | Divide Value Value
  | Subtract Value Value
  | Identifier Text
  | Inte Integer
  deriving (Show, Eq)
