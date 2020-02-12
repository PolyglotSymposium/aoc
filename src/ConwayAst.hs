module ConwayAst
       ( Problem(..)
       , SolvableConwayDimensions(..)
       , CellTransitions(..)
       , GenerationDirective(..)
       , CellIdent(..)
       , CellAlias(..)
       , aliasName
       , CellAliases
       ) where

import Data.Text
import qualified Ast

newtype CellIdent = CellIdent Char deriving (Show, Eq)
newtype CellAlias = CellAlias Text deriving (Show, Eq)

aliasName :: CellAlias -> Text
aliasName (CellAlias name) = name

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

data SolvableConwayDimensions
  = TwoD
  deriving (Show, Eq)

type CellAliases = [(CellIdent, CellAlias)]

data Problem =
  ConwayProblem
    -- TODO filepath
  { initialStateAt :: Text
  , dimensions :: SolvableConwayDimensions
  , cellAliases :: CellAliases
  , cellTransitions :: CellTransitions
  , solution :: GenerationDirective
  } deriving (Show, Eq)
