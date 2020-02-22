{-# LANGUAGE OverloadedStrings #-}

module ProgramProblem
       ( runProgramProblem
       ) where

import           Data.Text
import qualified ProgramAst as Ast
import qualified ProgramParser as Parse
import           Text.Megaparsec
import           Text.Megaparsec.Error (parseErrorPretty)
import qualified Type
import qualified Value as V

runProgramProblem :: (String, String) -> IO (Maybe (Type.Type, Type.Type, V.Value, Ast.Problem))
runProgramProblem (source, text) =
  case runParser Parse.programSpec source $ pack text of
    Left err -> do
      putStrLn "Error parsing aoc code file:"
      putStrLn $ parseErrorPretty err
      pure Nothing
    Right ast -> do
      print ast
      pure Nothing
