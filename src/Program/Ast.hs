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
       , Trace(..)
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
  | Noop
  deriving (Show, Eq)

data InstructionSpec
  = InstParts
  { terms::[ParseTerm]
  , meaning::Meaning
  , condition::Maybe Ast.Value
  }
  deriving (Show, Eq)

data Trace
  = TraceRegisterValues
  deriving (Show, Eq, Ord)

data Problem =
  ProgramProblem
  { programAt :: Text
  , instructions :: [InstructionSpec]
  , initialRegisterValue :: Integer
  , solution :: Ast.Solution
  , traces :: S.Set Trace
  , globalRegisters :: S.Set Text
  } deriving (Show, Eq)

newtype IndexedProgram
  = IndexedInstructions (M.Map Integer Instruction)
  deriving (Show, Eq)

indexed :: Program -> IndexedProgram
indexed (Instructions is) = IndexedInstructions $ M.fromList $ zip [0..] is

data Op
  = Set Text Ast.Value
  | JumpAway Ast.Value
  | DoNothing
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

inlineSpecs :: S.Set Text -> IntermediateProgram -> Program
inlineSpecs globalRegs (IntermediateInstructions iis) =
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
              SetRegister dest op' ->
                case (M.lookup (SpecName dest) (registers ii), S.member dest globalRegs) of
                  (Just reg, _) -> Set reg $ sub op'
                  (_, True) -> Set dest $ sub op'
                  _ -> error "Could not find register in global registers or instruction"

              RelativeJump name    -> JumpAway $ sub $ Ast.Identifier name
              Noop                 -> DoNothing
        , when = sub <$> specWhen ii
        }

allRegisters :: IntermediateProgram -> [Text]
allRegisters (IntermediateInstructions is) = S.toList $ S.fromList $ do
  instruction    <- is
  snd <$> M.toList (registers instruction)
