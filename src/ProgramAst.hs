{-# LANGUAGE OverloadedStrings #-}

module ProgramAst
       ( Problem(..)
       , ParseTerm(..)
       , Meaning(..)
       , InstPart(..)
       , Instruction(..)
       ) where

import qualified Ast
import           Data.Text

data ParseTerm
  = Literal Text
  | Register Text
  | Number Text
  deriving (Show, Eq)

data Meaning
  = SetRegister Text Ast.Value
  | RelativeJump Text
  deriving (Show, Eq)

data InstPart = InstPart { terms::[ParseTerm], meaning::Meaning, condition::Maybe Ast.Value }
  deriving (Show, Eq)

newtype Instruction
  = InstForm [InstPart]
  deriving (Show, Eq)

data Problem =
  ProgramProblem
  { programAt :: Text
  , initialRegisterValue :: Integer
  , instructions :: [Instruction]
  , solution :: Ast.Solution
  } deriving (Show, Eq)
