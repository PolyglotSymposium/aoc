{-# LANGUAGE OverloadedStrings #-}

module Aoc
       ( solve
       , SupportedDomainAst(..)
       ) where

import qualified ConwayAst as ConwayProblem
import qualified ConwayProblem
import           Data.Text (pack)
import qualified ListAst as ListProblem
import           ListProblem
import qualified Parser as P
import           Text.Megaparsec
import qualified Type
import qualified Value as V

data SupportedDomainAst
  = List ListProblem.Problem
  | Conway ConwayProblem.Problem

type Solver a = (String, String) -> IO (Maybe (Type.Type, Type.Type, V.Value, a))

toDomain :: (a -> SupportedDomainAst) -> Solver a -> Solver SupportedDomainAst
toDomain convert solver input = do
  ans <- solver input
  pure $ case ans of
    Just (it, ot, v, domain) -> Just (it, ot, v, convert domain)
    Nothing -> Nothing

solver :: P.Parser (Solver SupportedDomainAst)
solver =
  P.lstr "conway" *> pure (toDomain Conway ConwayProblem.runConwayProblem)
    <|> P.lstr "list" *> pure (toDomain List ListProblem.runListProblem)

solve :: String -> IO (Maybe (Type.Type, Type.Type, V.Value, SupportedDomainAst))
solve source = do
  text <- readFile source
  case runParser solver source $ pack text of
    Left err -> do
      putStrLn "Error parsing aoc code file:"
      putStrLn $ parseErrorPretty err
      pure Nothing

    Right solve' ->
      solve' (source, text)
