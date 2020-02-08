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
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

conway :: P.Parser Conway.Problem
conway = do
  _ <- P.lexeme $ P.ws *> string "conway"
  _ <- P.lstr "of"
  dim <- dimensions
  statePath <- initialStatePath
  aliases <- cellAliases
  transitions <- cellTransitions (snd <$> aliases)
  undefined

cellTransitions :: [Conway.CellAlias] -> P.Parser Conway.CellTransitions
cellTransitions aliases = do
  _ <- P.lstr "cells" *> P.lstr "transition"
  transitions <- some cellTransition
  _ <- P.lstr "otherwise" *> P.lstr "a" *> P.lstr "cell" *> P.lstr "is"
  cellDefault <- choice (P.lstr . Conway.aliasName <$> aliases)
  undefined

cellTransition :: P.Parser [(Conway.CellAlias, Conway.CellAlias, Conway.Value)]
cellTransition = undefined

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
  c <- Conway.CellIdent <$> P.lexeme (char '\'' *> asciiChar <* char '\'')
  _ <- P.lexeme "means"
  alias <- Conway.CellAlias <$> P.simpleQuoted
  pure (c, alias)
