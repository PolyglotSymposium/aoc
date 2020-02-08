{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConwayParser
       ( conway
       ) where

import qualified Ast as Conway
import qualified ConwayAst as Conway
import           Data.Text
import qualified Parser as P
import           Text.Megaparsec
import           Text.Megaparsec.Char

conway :: P.Parser Conway.Problem
conway = do
  _ <- P.ws *> P.lstr "conway" *> P.lstr "of"
  dim <- dimensions
  statePath <- initialStatePath
  aliases <- cellAliases
  transitions <- cellTransitions (snd <$> aliases)
  _ <- P.lstr "solution"
  code <- P.code
  eof
  pure $ Conway.ConwayProblem
    { Conway.initialStateAt=statePath
    , Conway.dimensions=dim
    , Conway.cellAliases=aliases
    , Conway.cellTransitions=transitions
    , Conway.solution=Conway.Solution code
    }

alias :: [Conway.CellAlias] -> P.Parser Conway.CellAlias
alias aliases =  Conway.CellAlias <$> choice (P.lstr . Conway.aliasName <$> aliases)

cellTransitions :: [Conway.CellAlias] -> P.Parser Conway.CellTransitions
cellTransitions aliases = do
  _ <- P.lstr "cells" *> P.lstr "transition"
  transitions <- some cellTransition
  _ <- P.lstr "otherwise" *> P.lstr "a" *> P.lstr "cell" *> P.lstr "is"
  cellDefault <- alias aliases
  pure $ Conway.CellTransitions
    { Conway.cases = transitions,
      Conway.otherwiseCellIs = cellDefault
    }

  where
    cellTransition :: P.Parser (Conway.CellAlias, Conway.CellAlias, Conway.Value)
    cellTransition = do
      _ <- P.lstr "from"
      previous <- alias aliases
      _ <- P.lstr "to"
      next <- alias aliases
      _ <- P.lstr "if"
      cond <- P.value
      pure (previous, next, cond)

dimensions :: P.Parser Conway.SolvableConwayDimensions
dimensions =
  P.lstr "2" *> P.lstr "dimensions" *> pure Conway.TwoD

initialStatePath :: P.Parser Text
initialStatePath =
  P.lstr "initial" *> P.lstr "state" *> P.lstr "at" *> P.filePath

cellAliases :: P.Parser [(Conway.CellIdent, Conway.CellAlias)]
cellAliases = do
  P.lstr "where" *> sepBy1 cellAlias (P.lstr "and")

cellAlias :: P.Parser (Conway.CellIdent, Conway.CellAlias)
cellAlias = P.lexeme $ do
  character <- Conway.CellIdent <$> P.lexeme (char '\'' *> asciiChar <* char '\'')
  _ <- P.lexeme "means"
  aliasAs <- Conway.CellAlias <$> P.simpleQuoted
  pure (character, aliasAs)
