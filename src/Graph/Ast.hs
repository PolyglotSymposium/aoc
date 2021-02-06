module Graph.Ast
       ( Problem(..)
       , ParseTerm(..)
       , PreprocessingStep(..)
       ) where

import qualified Ast
import           Data.Text

data ParseTerm
  = Literal Text
  | Number Text
  | Char Text
  | Text Text
  | Word Text
  | ManySepBy Text [ParseTerm]
  deriving (Show, Eq)

data PreprocessingStep
  = Strip Text
  deriving (Show, Eq)

data Problem =
  GraphProblem
  { at :: Text
  , preprocessing :: [PreprocessingStep]
  , solution :: Ast.Solution
  , parseTerms :: [ParseTerm]
  , fromEdge :: Ast.Value
  , toEdge :: Ast.Value
  } deriving (Show, Eq)
