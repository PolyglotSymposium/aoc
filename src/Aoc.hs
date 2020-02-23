{-# LANGUAGE OverloadedStrings #-}

module Aoc
       ( solve
       , SupportedDomainAst(..)
       ) where

import qualified ConwayAst as ConwayProblem
import qualified ConwayProblem
import           Data.Functor (($>))
import           Data.Text (pack)
import qualified ListAst as ListProblem
import           ListProblem
import qualified Parser as P
import qualified ProgramAst as ProgramProblem
import qualified ProgramProblem
import           Text.Megaparsec
import qualified Type
import qualified Value as V

data SupportedDomainAst
  = List ListProblem.Problem
  | Conway ConwayProblem.Problem
  | Program ProgramProblem.Problem

type Solver a = (String, String) -> IO (Maybe (Type.Type, Type.Type, V.Value, a))

toDomain :: (a -> SupportedDomainAst) -> Solver a -> Solver SupportedDomainAst
toDomain convert slvr input = do
  ans <- slvr input
  pure $ case ans of
    Just (it, ot, v, domain) -> Just (it, ot, v, convert domain)
    Nothing -> Nothing

solver :: P.Parser (Solver SupportedDomainAst)
solver =
  P.lstr "conway" $> toDomain Conway ConwayProblem.runConwayProblem
    <|> P.lstr "list" $> toDomain List ListProblem.runListProblem
    <|> P.lstr "program" $> toDomain Program ProgramProblem.runProgramProblem

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
