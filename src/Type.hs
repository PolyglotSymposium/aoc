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
  | Text
  | Grid
  | Graph
  | DijkstraOutputs
  | CellState
  | Position
  | Program
  | Register
  | Turtle
  | Direction
  | ParsedLine
  | Hidden
  deriving (Show, Eq)

