{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Turtle.Parser
       ( turtleSpec
       , turtleActions
       ) where

import qualified Data.Map.Strict as M
import           Data.Text hiding (concat)
import qualified Parser as P
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Turtle.Ast as Turtle

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


turtleActions :: Turtle.Problem -> P.Parser [Turtle.Action]
turtleActions Turtle.TurtleProblem{..} =
  concat <$> sepEndBy1 (action instructions) (() <$ P.lstr separator <|> eof)

action :: [Turtle.InstructionSpec] -> P.Parser [Turtle.Action]
action = choice . fmap op
  where
    op :: Turtle.InstructionSpec -> P.Parser [Turtle.Action]
    op spec = do
      ns <- M.unions <$> try (sequence (numbers <$> Turtle.terms spec))
      resolve (Turtle.actions spec) ns

    numbers :: Turtle.ParseTerm -> P.Parser (M.Map Text Integer)
    numbers (Turtle.Literal literal) = M.empty <$ P.lstr literal
    numbers (Turtle.Number name) = do
      value <- P.lexeme P.rawInteger
      pure $ M.singleton name value

    resolve :: [Turtle.ActionSpec] -> M.Map Text Integer -> P.Parser [Turtle.Action]
    resolve [] _ = pure []
    resolve (Turtle.ShouldFace d:rest) m = do
      resolved <- resolve rest m
      pure $ Turtle.Face d:resolved
    resolve (Turtle.ShouldTurn s:rest) m = do
      resolved <- resolve rest m
      pure $ Turtle.Turn s:resolved
    resolve (Turtle.ShouldTakeLiteralSteps s d:rest) m = do
      resolved <- resolve rest m
      pure $ Turtle.TakeSteps s d:resolved
    resolve (Turtle.ShouldTakeStepsIn name d:rest) m = do
      resolved <- resolve rest m
      case M.lookup name m of
        Just s -> pure $ Turtle.TakeSteps s d:resolved
        Nothing -> fail $ "Name " ++ unpack name ++ " was referenced on the right side of a `means` but not defined on the left side."

instructionsSpec :: P.Parser [Turtle.InstructionSpec]
instructionsSpec = sepEndBy1 (try instructionSpec) (P.lstr "and" <|> P.lstr "solution")

instructionSpec :: P.Parser Turtle.InstructionSpec
instructionSpec = Turtle.InstParts <$> manyTill term (P.lstr "means") <*> actionsSpec

actionsSpec :: P.Parser [Turtle.ActionSpec]
actionsSpec = sepBy1 actionSpec (P.lstr "then")

actionSpec :: P.Parser Turtle.ActionSpec
actionSpec =
  Turtle.ShouldFace <$> (P.lstr "face" *> direction)
    <|> Turtle.ShouldTurn <$> (P.lstr "turn" *> side)
    <|> takeSteps

takeSteps :: P.Parser Turtle.ActionSpec
takeSteps = do
  (P.lstr "take" *> (literalSteps <|> variableSteps) <* steps) <*> optional direction

  where
    literalSteps = Turtle.ShouldTakeLiteralSteps <$> P.lexeme P.rawInteger
    variableSteps = Turtle.ShouldTakeStepsIn <$> P.ident

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
