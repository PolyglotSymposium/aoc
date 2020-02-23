{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Program.Parser
       ( programSpec
       , program
       ) where

import qualified Ast as Program
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Text hiding (zip, maximum, length, foldr)
import qualified Parser as P
import qualified Program.Ast as Program
import           Text.Megaparsec
import           Text.Megaparsec.Char

programSpec :: P.Parser Program.Problem
programSpec = do
  _ <- P.ws *> P.lstr "program" *> P.lstr "at"
  programLocation <- P.filePath
  instructions <- instructionsSpec
  registerStartValue <- initialRegisterValue
  traces <- traceRules
  _ <- P.lstr "solution"
  code <- P.code
  eof
  pure $
    Program.ProgramProblem {
      Program.programAt = programLocation
    , Program.instructions = instructions
    , Program.initialRegisterValue = registerStartValue
    , Program.solution = code
    , Program.traces = traces
    }

traceRules :: P.Parser (S.Set Program.Trace)
traceRules =
  fromMaybe S.empty <$> optional (S.singleton Program.TraceRegisterValues <$ P.lstr "trace" <* P.lstr "register" <* P.lstr "values")

program :: Program.Problem -> P.Parser Program.IntermediateProgram
program spec = Program.IntermediateInstructions <$> manyTill (instruction $ Program.instructions spec) eof

instruction :: [Program.InstructionSpec] -> P.Parser Program.IntermediateInstruction
instruction = choice . fmap op
  where
    op :: Program.InstructionSpec -> P.Parser Program.IntermediateInstruction
    op spec = do
      (registers, numbers) <- merge <$> try (sequence (registersAndNumbers <$> Program.terms spec))
      pure $ Program.IntermediateInstruction
        { Program.registers = registers
        , Program.numbers   = numbers
        , Program.specOp    = Program.meaning spec
        , Program.specWhen  = Program.condition spec
        }

    registersAndNumbers :: Program.ParseTerm -> P.Parser (Program.Registers, Program.Numbers)
    registersAndNumbers (Program.Literal literal) = (M.empty, M.empty) <$ P.lstr literal
    registersAndNumbers (Program.Register name) = do
      reg <- P.lexeme P.ident
      pure (M.singleton (Program.SpecName name) reg, M.empty)
    registersAndNumbers (Program.Number name) = do
      value <- P.lexeme P.rawInteger
      pure (M.empty, M.singleton (Program.SpecName name) value)
    registersAndNumbers (Program.Val name) =
      registersAndNumbers (Program.Number name) <|> registersAndNumbers (Program.Register name)

    merge :: [(Program.Registers, Program.Numbers)] -> (Program.Registers, Program.Numbers)
    merge = foldr (\(r1, n1) (r2, n2) -> (M.union r1 r2, M.union n1 n2)) (M.empty, M.empty)

initialRegisterValue :: P.Parser Integer
initialRegisterValue =
  P.lstr "start" *> P.lstr "at" *> P.lexeme P.rawInteger

instructionsSpec :: P.Parser [Program.InstructionSpec]
instructionsSpec =
  P.lstr "with" *> P.lstr "instructions" *> manyTill instructionSpec (P.lstr "registers")

instructionSpec :: P.Parser Program.InstructionSpec
instructionSpec = Program.InstParts <$> manyTill term (P.lstr "means") <*> meaning <*> optional condition

condition :: P.Parser Program.Value
condition = P.lstr "if" *> P.value

term :: P.Parser Program.ParseTerm
term = P.lexeme (
   between (char '{') (char '}') numberOrRegister
     <|> Program.Literal <$> P.ident
     <|> Program.Literal . pack <$> some (punctuationChar <|> symbolChar)
   )

  where
    numberOrRegister :: P.Parser Program.ParseTerm
    numberOrRegister = do
      ident <- P.ident
      _ <- char ':'
      ty <- choice [P.lstr "reg", P.lstr "num", P.lstr "val"]
      case ty of
        "reg" -> pure $ Program.Register ident
        "num" -> pure $ Program.Number ident
        "val" -> pure $ Program.Val ident
        _     -> fail ("Unexpected type in program spec: " ++ show ty)

meaning :: P.Parser Program.Meaning
meaning = setOrJump
  where
    setOrJump =
      Program.SetRegister <$> (P.lstr "set" *> P.ident) <*> (P.lstr "to" *> P.value)
        <|> Program.RelativeJump <$> (P.lstr "jump" *> P.ident <* P.lstr "away")
