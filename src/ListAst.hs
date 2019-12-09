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
  } deriving Show

data Solution
  = Pipe Solution Solution
  | For Lambda Lambda (Maybe Lambda)
  | FloatingLambda Lambda
  deriving Show

newtype Lambda
  = Body Value
  deriving Show

data Value
  = Gt Value Value
  | Divide Value Value
  | Subtract Value Value
  | Identifier Text
  | Inte Integer
  deriving Show
