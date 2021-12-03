module Turtle.Ast
       ( Problem(..)
       , SolvableDimensions(..)
       , ParseTerm(..)
       , Direction(..)
       , Side(..)
       , Action(..)
       , ActionSpec(..)
       , InstructionSpec(..)
       , AdditionalState(..)
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
  deriving (Show, Eq, Ord)

data Side
  = Lefthand
  | Righthand
  deriving (Show, Eq)

data Action
  = Face Direction
  | Turn Side
  | TakeSteps (Either Integer Ast.Value) (Maybe Direction)
  | SetState Text Ast.Value
  deriving (Show, Eq)

data ActionSpec
  = ShouldFace Direction
  | ShouldTurn Side
  | ShouldTakeLiteralSteps Integer (Maybe Direction)
  | ShouldTakeCalculatedSteps Ast.Value (Maybe Direction)
  | ShouldTakeStepsIn      Text (Maybe Direction)
  | ShouldSetState Text Ast.Value
  deriving (Show, Eq)

newtype AdditionalState = AdditionalState (Maybe (Text, Integer))
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
  , additionalState :: AdditionalState
  } deriving (Show, Eq)
