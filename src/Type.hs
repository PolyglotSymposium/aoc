module Type
       ( Type(..)
       ) where

data Type
  = Number
  | Boolean
  | List Type
  | Arrow Type Type
  | Var Char
  | Grid
  | CellState
  | Position
  | Program
  | Register
  deriving (Show, Eq)

