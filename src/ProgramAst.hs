{-# LANGUAGE OverloadedStrings #-}

module ProgramAst
       ( Problem(..)
       , ParseTerm(..)
       , Meaning(..)
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

data Instruction
  = InstParts { terms::[ParseTerm], meaning::Meaning, condition::Maybe Ast.Value }
  deriving (Show, Eq)

data Problem =
  ProgramProblem
  { programAt :: Text
  , instructions :: [Instruction]
  , initialRegisterValue :: Integer
  , solution :: Ast.Solution
  } deriving (Show, Eq)
