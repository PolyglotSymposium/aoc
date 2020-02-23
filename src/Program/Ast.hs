module Program.Ast
       ( Problem(..)
       , ParseTerm(..)
       , Meaning(..)
       , InstructionSpec(..)
       , Instruction(..)
       , Program(..)
       , IndexedProgram(..)
       , allRegisters
       , indexed
       , inlineSpecs
       , Registers
       , Numbers
       , NameInSpec(..)
       , IntermediateProgram(..)
       , IntermediateInstruction(..)
       , Op(..)
       ) where

import qualified Ast
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Text hiding (zip)

data ParseTerm
  = Literal Text
  | Register Text
  | Number Text
  | Val Text
  deriving (Show, Eq)

data Meaning
  = SetRegister Text Ast.Value
  | RelativeJump Text
  deriving (Show, Eq)

data InstructionSpec
  = InstParts
  { terms::[ParseTerm]
  , meaning::Meaning
  , condition::Maybe Ast.Value
  }
  deriving (Show, Eq)

data Problem =
  ProgramProblem
  { programAt :: Text
  , instructions :: [InstructionSpec]
  , initialRegisterValue :: Integer
  , solution :: Ast.Solution
  } deriving (Show, Eq)

newtype IndexedProgram
  = IndexedInstructions (M.Map Integer Instruction)
  deriving (Show, Eq)

indexed :: Program -> IndexedProgram
indexed (Instructions is) = IndexedInstructions $ M.fromList $ zip [0..] is

data Op
  = Set Text Ast.Value
  | JumpAway Ast.Value
  deriving (Show, Eq)

data Instruction =
  Instruction
  { op   :: Op
  , when :: Maybe Ast.Value }
  deriving (Show, Eq)

newtype Program
  = Instructions [Instruction]
  deriving (Show, Eq)

newtype NameInSpec
  = SpecName Text
  deriving (Show, Eq, Ord)

specName :: NameInSpec -> Text
specName (SpecName name) = name

type Registers = M.Map NameInSpec Text

type Numbers = M.Map NameInSpec Integer

newtype IntermediateProgram
  = IntermediateInstructions [IntermediateInstruction]
  deriving (Show, Eq)

data IntermediateInstruction =
  IntermediateInstruction
  { registers :: Registers
  , numbers   :: Numbers
  , specOp    :: Meaning
  , specWhen  :: Maybe Ast.Value }
  deriving (Show, Eq)

inlineSpecs :: IntermediateProgram -> Program
inlineSpecs (IntermediateInstructions iis) =
  Instructions $ inlineSpec <$> iis

  where
    inlineSpec ii =
      let
        keysThen f g = M.map g $ M.mapKeys specName $ f ii
        sub = Ast.substitute (M.union (keysThen registers Ast.Identifier) (keysThen numbers Ast.Inte))
      in
        Instruction
        { op =
            case specOp ii of
              SetRegister dest op' -> Set (registers ii M.! SpecName dest) $ sub op'
              RelativeJump name   -> JumpAway $ sub $ Ast.Identifier name
        , when = sub <$> specWhen ii
        }

allRegisters :: IntermediateProgram -> [Text]
allRegisters (IntermediateInstructions is) = S.toList $ S.fromList $ do
  instruction    <- is
  snd <$> M.toList (registers instruction)
