module Type
       ( Type(..)
       ) where

data Type
  = Number
  | Boolean
  | List Type
  | Arrow Type Type
  | Var Char
  | Char
  | Grid
  | CellState
  | Position
  | Program
  | Register
  | Turtle
  | Direction
  | ParsedLine
  deriving (Show, Eq)

