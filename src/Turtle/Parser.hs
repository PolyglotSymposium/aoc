{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Turtle.Parser
       ( turtleSpec
       , listInput
       ) where

import           Data.Text
import qualified Parser as P
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Turtle.Ast as Turtle
import qualified Value as V

listInput :: Text -> P.Parser V.Value -> P.Parser [V.Value]
listInput sep pValue = P.ws *> sepBy pValue (P.lstr sep) <* eof

turtleSpec :: P.Parser Turtle.Problem
turtleSpec = do
  _ <- P.ws *> P.lstr "turtle" *> P.lstr "in"
  dim <- dimensions
  _ <- P.lstr "at"
  file <- P.filePath
  _ <- P.lstr "separated" *> P.lstr "by"
  separator <- P.simpleQuoted
  _ <- P.lstr "where"
  instructions <- instructionsSpec
  code <- P.code
  eof
  pure $
    Turtle.TurtleProblem {
      Turtle.at           = file
    , Turtle.separator    = separator
    , Turtle.solution     = code
    , Turtle.dimensions   = dim
    , Turtle.instructions = instructions
    }

instructionsSpec :: P.Parser [Turtle.InstructionSpec]
instructionsSpec = manyTill instructionSpec (P.lstr "solution")

instructionSpec :: P.Parser Turtle.InstructionSpec
instructionSpec = Turtle.InstParts <$> manyTill term (P.lstr "means") <*> actions

{-
( means face up then take 1 step and
) means face down then take 1 step
-}

actions :: P.Parser [Turtle.ActionSpec]
actions = sepBy1 (try action) (P.lstr "then")

action :: P.Parser Turtle.ActionSpec
action =
  Turtle.Face <$> (P.lstr "face" *> direction)
    <|> Turtle.Turn <$> (P.lstr "turn" *> side)
    <|> Turtle.TakeLiteralSteps <$> (P.lstr "take" *> P.lexeme P.rawInteger <* steps)

steps :: P.Parser ()
steps = () <$ P.lexeme (string "step" *> optional (char 's'))

side :: P.Parser Turtle.Side
side =
  Turtle.Lefthand <$ P.lstr "left"
    <|> Turtle.Righthand <$ P.lstr "right"

direction :: P.Parser Turtle.Direction
direction =
  Turtle.Up <$ P.lstr  "up"
    <|> Turtle.Up <$ P.lstr "north"
    <|> Turtle.Left <$ P.lstr "left"
    <|> Turtle.Left <$ P.lstr "west"
    <|> Turtle.Right <$ P.lstr "right"
    <|> Turtle.Right <$ P.lstr "east"
    <|> Turtle.Down <$ P.lstr "down"
    <|> Turtle.Down <$ P.lstr "south"

term :: P.Parser Turtle.ParseTerm
term = P.lexeme (
   between (char '{') (char '}') number
     <|> Turtle.Literal <$> P.ident
     <|> Turtle.Literal . pack <$> some (punctuationChar <|> symbolChar)
   )

  where
    number :: P.Parser Turtle.ParseTerm
    number = do
      ident <- P.ident
      _ <- char ':'
      ty <- choice [P.lstr "reg", P.lstr "num", P.lstr "val"]
      case ty of
        "num" -> pure $ Turtle.Number ident
        _     -> fail ("Unexpected type in turtle spec: " ++ show ty)

dimensions :: P.Parser Turtle.SolvableDimensions
dimensions =
  P.lstr "2" *> P.lstr "dimensions" *> pure Turtle.TwoD <|>
  P.lstr "1" *> P.lstr "dimension" *> pure Turtle.OneD
