module ListAst
       ( Problem(..)
       ) where

import Data.Text
import qualified Ast as Ast

data Problem =
  ListProblem
  { at :: Text
  , separator :: Text
  , solution :: Ast.Solution
  } deriving (Show, Eq)
