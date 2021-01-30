module List.Ast
       ( Problem(..)
       , ParseTerm(..)
       ) where

import qualified Ast
import           Data.Text

-- TODO: Unify with parse terms from program domain
data ParseTerm
  = Literal Text
  | Number Text
  deriving (Show, Eq)

data Problem =
  ListProblem
  { at :: Text
  , separator :: Text
  , solution :: Ast.Solution
  , parseTerms :: [ParseTerm]
  } deriving (Show, Eq)
