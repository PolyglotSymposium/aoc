{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Conway.Parser
       ( conway
       , singleLayerFiniteConwayInput
       , singleLayerInfiniteConwayInput
       ) where

import qualified Ast as Conway
import qualified Conway.Ast as Conway
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe, listToMaybe)
import           Data.Text hiding (zip, maximum, length)
import           Control.Monad (guard)
import qualified Parser as P
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Value as V

conway :: P.Parser Conway.Problem
conway = do
  _ <- P.ws *> P.lstr "conway" *> P.lstr "of"
  dim <- dimensions
  isInf <- optional infiniteClause
  statePath <- initialStatePath
  (aliases, emptinessCell) <- cellAliases
  infEmptyCell <- infiniteEmptinessCell isInf emptinessCell
  transitions <- cellTransitions aliases
  oob <- optional $ try $ outOfBoundsCells aliases
  directive <- generationDirective
  eof
  pure $ Conway.ConwayProblem
    { Conway.initialStateAt=statePath
    , Conway.dimensions=dim
    , Conway.cellAliases=aliases
    , Conway.cellTransitions=transitions
    , Conway.solution=directive
    , Conway.outOfBoundsCellsAre=oob
    , Conway.infiniteEmptinessCell=infEmptyCell
    }

  where
    infiniteEmptinessCell (Just _) Nothing =
      fail "Infinite grids must have a cell representing emptiness"
    infiniteEmptinessCell inf i = pure $ inf >> i

generationDirective :: P.Parser Conway.GenerationDirective
generationDirective =
  (Conway.Solution <$> (P.lstr "solution" *> P.code)) <|>
  try (Conway.Animate Conway.Forever <$ (P.lstr "animate" *> P.lstr "forever")) <|>
  (Conway.Animate . Conway.Generations <$> (P.lstr "animate" *> P.lexeme P.rawInteger <* P.lstr "generations"))

outOfBoundsCells :: Conway.CellAliases -> P.Parser Conway.CellIdent
outOfBoundsCells aliases =
  P.lstr "an" *> P.lstr "out-of-bounds" *> P.lstr "cell" *> P.lstr "is" *> alias aliases

singleLayerFiniteConwayInput :: Conway.CellTransitions -> Conway.CellAliases -> P.Parser V.Value
singleLayerFiniteConwayInput transitions aliases = do
  rows <- grid
  let cells = positionCells rows
  let width = maximum $ fmap ((+ 1) . fst . fst) cells
  let height = maximum $ fmap ((+ 1) . snd . fst) cells
  pure $ V.Grid transitions (V.WidthHeight{ V.width=width, V.height=height }) $ M.fromList cells

  where
    positionCells rows = do
      (y, line) <- zip [0..] rows
      (x, cell) <- zip [0..] line
      [((x, y), cell)]

    grid :: P.Parser [String]
    grid = endBy1 (some cellState) (P.ws <|> eof)

    cellState :: P.Parser Char
    cellState = choice ((\(Conway.CellIdent c, _) -> char c) <$> aliases)

singleLayerInfiniteConwayInput :: Conway.SolvableConwayDimensions -> Conway.CellIdent -> Conway.CellTransitions -> Conway.CellAliases -> P.Parser V.Value
singleLayerInfiniteConwayInput dim (Conway.CellIdent emptinessCell) transitions aliases = do
  rows <- grid
  let cells = positionCells rows
  pure $ V.InfiniteGrid transitions dim emptinessCell $ M.fromList cells

  where
    positionCells rows = do
      (y, line) <- zip [0..] rows
      (x, cell) <- zip [0..] line
      guard $ cell /= emptinessCell
      [(toCoord dim (x, y), cell)]

    toCoord Conway.OneD (x, _) = V.D1 x
    toCoord Conway.TwoD (x, y) = V.D2 x y
    toCoord Conway.ThreeD (x, y) = V.D3 x y 0

    grid :: P.Parser [String]
    grid = endBy1 (some cellState) (P.ws <|> eof)

    cellState :: P.Parser Char
    cellState = choice ((\(Conway.CellIdent c, _) -> char c) <$> aliases)

alias :: Conway.CellAliases -> P.Parser Conway.CellIdent
alias aliases = choice $ identFromAlias <$> aliases
  where
    identFromAlias (ident, Conway.CellAlias al) =
      ident <$ P.lstr al

cellTransitions :: Conway.CellAliases -> P.Parser Conway.CellTransitions
cellTransitions aliases = do
  _ <- P.lstr "cells" *> P.lstr "transition"
  transitions <- some cellTransition
  _ <- P.lstr "otherwise" *> P.lstr "a" *> P.lstr "cell" *> P.lstr "is"
  cellDefault <- Conway.Unchanged <$ try (P.lstr "unchanged") <|> Conway.DefaultCell <$> alias aliases
  pure $ Conway.CellTransitions
    { Conway.cases = transitions,
      Conway.otherwiseCellIs = cellDefault
    }

  where
    cellTransition :: P.Parser (Conway.CellIdent, Conway.CellIdent, Conway.Value)
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
  ["3", "dimensions"] `P.phraseAs` Conway.ThreeD <|>
  ["2", "dimensions"] `P.phraseAs` Conway.TwoD <|>
  ["1", "dimension"] `P.phraseAs` Conway.OneD

infiniteClause :: P.Parser ()
infiniteClause = P.phrase ["and", "infinite", "size"]

initialStatePath :: P.Parser Text
initialStatePath = P.phrase ["initial", "state", "at"] *> P.filePath

type AliasOrEmptiness = Either (Conway.CellIdent, Conway.CellAlias) Conway.CellIdent

cellAliases :: P.Parser ([(Conway.CellIdent, Conway.CellAlias)], Maybe Conway.CellIdent)
cellAliases = values
  where
    values = do
      vs <- P.lstr "where" *> sepBy1 cellAliasOrEmptiness (P.lstr "and")
      case (toAlias <$> vs, mapMaybe emptinessIdent vs) of
        ([], _) -> fail "Must have at least one cell alias"
        (_, _:_:_) -> fail "Must not have more than one cell representing emptiness"
        (aliases, emptiness) -> pure (aliases, listToMaybe emptiness)

    toAlias = \case
      Left (i, a) -> (i, a)
      Right i -> (i, Conway.CellAlias "emptiness")

    emptinessIdent = either (const Nothing) Just

cellAliasOrEmptiness :: P.Parser AliasOrEmptiness
cellAliasOrEmptiness = P.lexeme $ do
  character <- Conway.CellIdent <$> P.lexeme (char '\'' *> asciiChar <* char '\'')
  alias' character <|> emptiness character

  where
    alias' character = do
      _ <- P.lexeme "means"
      aliasAs <- Conway.CellAlias <$> P.simpleQuoted
      pure $ Left (character, aliasAs)

    emptiness character =
      ["represents", "emptiness"] `P.phraseAs` Right character
