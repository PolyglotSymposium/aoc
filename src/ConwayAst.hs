module ConwayAst
       ( Problem(..)
       , SolvableConwayDimensions(..)
       , CellTransitions(..)
       , GenerationDirective(..)
       , CellIdent(..)
       , CellAlias(..)
       , aliasName
       , ident
       , CellAliases
       , transitionCases
       ) where

import Data.Text
import qualified Ast

newtype CellIdent = CellIdent Char deriving (Show, Eq)
newtype CellAlias = CellAlias Text deriving (Show, Eq)

aliasName :: CellAlias -> Text
aliasName (CellAlias name) = name

ident :: CellIdent -> Char
ident (CellIdent c) = c

trd :: (a, b, c) -> c
trd (_, _, t) = t

transitionCases :: CellTransitions -> [Ast.Value]
transitionCases (CellTransitions cases _) = trd <$> cases

data CellTransitions = CellTransitions
  { cases :: [(CellIdent, CellIdent, Ast.Value)]
  , otherwiseCellIs :: CellIdent
  }
  deriving (Show, Eq)

data GenerationDirective
  = Solution Ast.Solution
  deriving (Show, Eq)

data SolvableConwayDimensions
  = TwoD
  deriving (Show, Eq)

type CellAliases = [(CellIdent, CellAlias)]

data Problem =
  ConwayProblem
  { initialStateAt :: Text
  , dimensions :: SolvableConwayDimensions
  , cellAliases :: CellAliases
  , cellTransitions :: CellTransitions
  , solution :: GenerationDirective
  } deriving (Show, Eq)
