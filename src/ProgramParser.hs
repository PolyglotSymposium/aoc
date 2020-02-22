{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProgramParser
       ( programSpec
       ) where

import qualified Ast as Program
import           Data.Functor (void)
import qualified Data.Map.Strict as M
import           Data.Text hiding (zip, maximum, length)
import qualified Parser as P
import qualified ProgramAst as Program
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Value as V

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

initialRegisterValue :: P.Parser Integer
initialRegisterValue =
  P.lstr "start" *> P.lstr "at" *> P.lexeme P.rawInteger

instructionsSpec :: P.Parser [Program.Instruction]
instructionsSpec =
  P.lstr "with" *> P.lstr "instructions" *> manyTill instruction (P.lstr "registers")

instruction :: P.Parser Program.Instruction
instruction = Program.InstParts <$> manyTill term (P.lstr "means") <*> meaning <*> optional condition

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
      pure $ case ty of
        "reg" -> Program.Register ident
        "num" -> Program.Number ident

meaning :: P.Parser Program.Meaning
meaning = setOrJump
  where
    setOrJump =
      Program.SetRegister <$> (P.lstr "set" *> P.ident) <*> (P.lstr "to" *> P.value)
        <|> Program.RelativeJump <$> (P.lstr "jump" *> P.ident <* P.lstr "away")
