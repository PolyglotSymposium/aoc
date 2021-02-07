module Graph.Ast
       ( Problem(..)
       , ParseTerm(..)
       , PreprocessingStep(..)
       , NodeTerms(..)
       , terms
       ) where

import qualified Ast
import           Data.Text

data ParseTerm
  = Literal Text
  | Number Text
  | Char Text
  | Text Text
  deriving (Show, Eq)

data PreprocessingStep
  = Strip Text
  deriving (Show, Eq)

data NodeTerms
  = SingleNode [ParseTerm]
  | ManyNodesSepBy Text [ParseTerm]
  deriving (Show, Eq)

terms :: NodeTerms -> [ParseTerm]
terms (SingleNode ts) = ts
terms (ManyNodesSepBy _ ts) = ts

data Problem =
  GraphProblem
  { at :: Text
  , preprocessing :: [PreprocessingStep]
  , edgeDesignator :: Text
  , leftTerms :: NodeTerms
  , rightTerms :: NodeTerms
  , fromNode :: Text
  , toNode :: Text
  , isBidirectional :: Bool
  , solution :: Ast.Solution
  } deriving (Show, Eq)
