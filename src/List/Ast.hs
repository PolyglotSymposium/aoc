module List.Ast
       ( Problem(..)
       , ParseTerm(..)
       ) where

import qualified Ast
import           Data.Text

data ParseTerm
  = Literal Text
  | Number Text
  | Char Text
  | Text Text
  deriving (Show, Eq)

data Problem =
  ListProblem
  { at :: Text
  , separator :: Text
  , solution :: Ast.Solution
  , parseTerms :: [ParseTerm]
  } deriving (Show, Eq)
