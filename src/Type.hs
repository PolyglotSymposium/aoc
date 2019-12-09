module Type
       ( Type(..)
       ) where

data Type
  = Number
  | Boolean
  | List Type
  | Arrow Type Type
  | Variable
  deriving (Show, Eq)

