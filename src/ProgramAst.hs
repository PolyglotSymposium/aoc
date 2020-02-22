{-# LANGUAGE OverloadedStrings #-}

module ProgramAst
       ( Problem(..)
       , ParseTerm(..)
       , Meaning(..)
       , InstructionSpec(..)
       , Instruction(..)
       , Program(..)
       , Registers
       , Numbers
       , NameInSpec(..)
       ) where

import qualified Ast
import qualified Data.Map.Strict as M
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

data InstructionSpec
  = InstParts { terms::[ParseTerm], meaning::Meaning, condition::Maybe Ast.Value }
  deriving (Show, Eq)

data Problem =
  ProgramProblem
  { programAt :: Text
  , instructions :: [InstructionSpec]
  , initialRegisterValue :: Integer
  , solution :: Ast.Solution
  } deriving (Show, Eq)

newtype Program
  = Instructions [Instruction]
  deriving (Show, Eq)

newtype NameInSpec
  = SpecName Text
  deriving (Show, Eq, Ord)

type Registers = M.Map NameInSpec Text

type Numbers = M.Map NameInSpec Integer

data Instruction =
  Instruction
  { registers::Registers
  , numbers::Numbers
  , op::Meaning
  , when::Maybe Ast.Value }
  deriving (Show, Eq)
