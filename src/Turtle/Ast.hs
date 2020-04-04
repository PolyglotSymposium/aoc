module Turtle.Ast
       ( Problem(..)
       , SolvableDimensions(..)
       , ParseTerm(..)
       , Direction(..)
       , Side(..)
       , ActionSpec(..)
       , InstructionSpec(..)
       ) where

import qualified Ast
import           Data.Text

data SolvableDimensions
  = TwoD
  | OneD
  deriving (Show, Eq)

data ParseTerm
  = Literal Text
  | Number Text
  deriving (Show, Eq)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Eq)

data Side
  = Lefthand
  | Righthand
  deriving (Show, Eq)

data ActionSpec
  = Face Direction
  | Turn Side
  | TakeLiteralSteps Integer
  | TakeStepsIn      Text
  deriving (Show, Eq)

data InstructionSpec
  = InstParts
  { terms :: [ParseTerm]
  , actions :: [ActionSpec]
  }
  deriving (Show, Eq)

data Problem =
  TurtleProblem
  { at :: Text
  , dimensions   :: SolvableDimensions
  , separator    :: Text
  , instructions :: [InstructionSpec]
  , solution     :: Ast.Solution
  } deriving (Show, Eq)
