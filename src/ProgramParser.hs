{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProgramParser
       ( programSpec
       , program
       ) where

import qualified Ast as Program
import qualified Data.Map.Strict as M
import           Data.Text hiding (zip, maximum, length, foldr)
import qualified Parser as P
import qualified ProgramAst as Program
import           Text.Megaparsec
import           Text.Megaparsec.Char

programSpec :: P.Parser Program.Problem
programSpec = do
  _ <- P.ws *> P.lstr "program" *> P.lstr "at"
  programLocation <- P.filePath
  instructions <- instructionsSpec
  registerStartValue <- initialRegisterValue
  _ <- P.lstr "solution"
  code <- P.code
  eof
  pure $
    Program.ProgramProblem {
      Program.programAt = programLocation
    , Program.instructions = instructions
    , Program.initialRegisterValue = registerStartValue
    , Program.solution = code
    }

program :: Program.Problem -> P.Parser Program.Program
program spec = Program.Instructions <$> manyTill (instruction $ Program.instructions spec) eof

instruction :: [Program.InstructionSpec] -> P.Parser Program.Instruction
instruction = choice . fmap op
  where
    op :: Program.InstructionSpec -> P.Parser Program.Instruction
    op spec = do
      (registers, numbers) <- merge <$> sequence (registersAndNumbers <$> Program.terms spec)
      pure $ Program.Instruction
        { Program.registers = registers
        , Program.numbers   = numbers
        , Program.op        = Program.meaning spec
        , Program.when      = Program.condition spec
        }

    registersAndNumbers :: Program.ParseTerm -> P.Parser (Program.Registers, Program.Numbers)
    registersAndNumbers (Program.Literal literal) = (M.empty, M.empty) <$ P.lstr literal
    registersAndNumbers (Program.Register name) = do
      reg <- P.ident
      pure (M.singleton (Program.SpecName name) reg, M.empty)
    registersAndNumbers (Program.Number name) = do
      value <- P.lexeme P.rawInteger
      pure (M.empty, M.singleton (Program.SpecName name) value)

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
     <|> Program.Literal . singleton <$> punctuationChar
   )

  where
    numberOrRegister :: P.Parser Program.ParseTerm
    numberOrRegister = do
      ident <- P.ident
      _ <- char ':'
      ty <- choice [P.lstr "reg", P.lstr "num"]
      case ty of
        "reg" -> pure $ Program.Register ident
        "num" -> pure $ Program.Number ident
        _     -> fail ("Unexpected type in program spec: " ++ show ty)

meaning :: P.Parser Program.Meaning
meaning = setOrJump
  where
    setOrJump =
      Program.SetRegister <$> (P.lstr "set" *> P.ident) <*> (P.lstr "to" *> P.value)
        <|> Program.RelativeJump <$> (P.lstr "jump" *> P.ident <* P.lstr "away")
