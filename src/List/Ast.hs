module List.Ast
       ( Problem(..)
       ) where

import qualified Ast
import           Data.Text

data Problem =
  ListProblem
  { at :: Text
  , separator :: Text
  , solution :: Ast.Solution
  } deriving (Show, Eq)
