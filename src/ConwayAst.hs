module ConwayAst
       ( Problem(..)
       ) where

import Data.Text
import qualified Ast

newtype CellIdent = CellIdent Char deriving (Show, Eq)
newtype CellAlias = CellAlias Text deriving (Show, Eq)

data CellTransitions = CellTransitions
  { cases :: [(CellAlias, CellAlias, Ast.Value)]
  , otherwiseCellIs :: CellAlias
  }
  deriving (Show, Eq)

data GenerationDirective
  = Solution Ast.Solution
  -- TODO
  -- | Animate
  deriving (Show, Eq)

data Problem =
  ConwayProblem
    -- TODO filepath
  { initialStateAt :: Text
  , cellAliases :: [(CellIdent, CellAlias)]
  , cellTransitions :: CellTransitions
  , solution :: GenerationDirective
  } deriving (Show, Eq)
