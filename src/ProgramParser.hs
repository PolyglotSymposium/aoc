{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProgramParser
       ( programSpec
       ) where

import qualified Ast as Program
import qualified ProgramAst as Program
import qualified Data.Map.Strict as M
import           Data.Text hiding (zip, maximum, length)
import qualified Parser as P
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Value as V

programSpec :: P.Parser Program.Problem
programSpec = do
  _ <- P.ws *> P.lstr "program" *> P.lstr "at"
  programLocation <- P.filePath
  instructions <- instructionSpec
  initialRegister <- initialRegisterValue
  _ <- P.lstr "solution"
  code <- P.code
  eof
  pure $
    Program.ProgramProblem {
      Program.programAt = programLocation
    , Program.initialRegisterValue = initialRegister
    , Program.instructions = instructions
    , Program.solution = code
    }

initialRegisterValue :: P.Parser Integer
initialRegisterValue =
  P.lstr "registers" *> P.lstr "start" *> P.lstr "at" *> P.lstr "value" *> P.rawInteger

-- jie {r:reg}, {offset:num} means jump offset away if (even r)
instructionSpec :: P.Parser [Program.Instruction]
instructionSpec = do
  P.lstr "with" *> P.lstr "instructions" *> sepBy1 instruction P.ws

instruction :: P.Parser Program.Instruction
instruction = Program.InstForm <$> sepBy1 instructionPart P.ws

instructionPart :: P.Parser Program.InstPart
instructionPart =
  Program.InstPart <$> sepBy1 term P.ws <*> meaning <*> optional condition

condition :: P.Parser Program.Value
condition = P.lstr "if" *> P.value

term :: P.Parser Program.ParseTerm
term =
   between (char '{') (char '}') numberOrRegister
     <|> Program.Literal <$> P.ident

  where
    numberOrRegister :: P.Parser Program.ParseTerm
    numberOrRegister = do
      ident <- P.ident
      _ <- char ':'
      ty <- choice [P.lstr "reg", P.lstr "num"]
      pure $ case ty of
        "reg" -> Program.Register ident
        "num" -> Program.Number ident

meaning :: P.Parser Program.Meaning
meaning = P.lstr "means" *> setOrJump
  where
    setOrJump =
      Program.SetRegister <$> (P.lstr "set" *> P.ident) <*> (P.lstr "to" *> P.value)
        <|> Program.RelativeJump <$> (P.lstr "jump" *> P.ident <* P.lstr "away")
