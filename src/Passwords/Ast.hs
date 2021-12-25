module Passwords.Ast
  ( Problem(..)
  , ValidityRule(..)
  , Amount(..)
  , SubstringMatch(..)
  ) where

import qualified Ast
import           Data.Text

data Amount
  = Any
  | AtLeast Integer
  deriving (Show, Eq)

data SubstringMatch
  = AnyCharOf Text
  | AnySubstringOf [Text]
  | SubstringMatching Text
  | SubstringGreedilyMatching Text
  deriving (Show, Eq)

data ValidityRule
  = Contain Amount SubstringMatch
  | DoNotContain Amount SubstringMatch
  deriving (Show, Eq)

data Problem = PasswordsProblem
  { at            :: Text
  , validityRules :: [ValidityRule]
  , solution      :: Ast.Solution
  }
  deriving (Show, Eq)
