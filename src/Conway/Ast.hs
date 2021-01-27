{-# LANGUAGE OverloadedStrings #-}

module Conway.Ast
       ( Problem(..)
       , SolvableConwayDimensions(..)
       , CellTransitions(..)
       , GenerationDirective(..)
       , AnimationDirective(..)
       , CellIdent(..)
       , CellAlias(..)
       , OtherwiseCell(..)
       , aliasName
       , ident
       , CellAliases
       , transitionCases
       , nextGenerationSolution
       ) where

import Data.Text
import qualified Ast

newtype CellIdent = CellIdent Char deriving (Show, Eq, Ord)
newtype CellAlias = CellAlias Text deriving (Show, Eq)

data OtherwiseCell
  = DefaultCell CellIdent
  | Unchanged
  deriving (Show, Eq)

aliasName :: CellAlias -> Text
aliasName (CellAlias name) = name

ident :: CellIdent -> Char
ident (CellIdent c) = c

trd :: (a, b, c) -> c
trd (_, _, t) = t

transitionCases :: CellTransitions -> [Ast.Value]
transitionCases (CellTransitions cs _) = trd <$> cs

data CellTransitions = CellTransitions
  { cases :: [(CellIdent, CellIdent, Ast.Value)]
  , otherwiseCellIs :: OtherwiseCell
  }
  deriving (Show, Eq)

data AnimationDirective
  = Forever
  | Generations Integer
  deriving (Show, Eq)

data GenerationDirective
  = Solution Ast.Solution
  | Animate AnimationDirective
  deriving (Show, Eq)

data SolvableConwayDimensions
  = OneD
  | TwoD
  | ThreeD
  | FourD
  deriving (Show, Eq, Ord)

type CellAliases = [(CellIdent, CellAlias)]

data Problem =
  ConwayProblem
  { initialStateAt :: Text
  , dimensions :: SolvableConwayDimensions
  , cellAliases :: CellAliases
  , cellTransitions :: CellTransitions
  , outOfBoundsCellsAre :: Maybe CellIdent
  , solution :: GenerationDirective
  , emptinessCell :: Maybe CellIdent
  , gridIsInfinite :: Bool
  } deriving (Show, Eq)

nextGenerationSolution :: Ast.Solution
nextGenerationSolution =
  Ast.FloatingLambda $ Ast.Body $ Ast.Identifier "next_generation"
