module Main (main) where

import qualified Aoc as Aoc
import qualified System.Environment as Env

main :: IO ()
main = do
  arg <- Env.getArgs
  case arg of
    [inputFile] -> do
      () <$ Aoc.runListProblem inputFile
    [inputFile, "--more-info"] -> do
      out <- Aoc.runListProblem inputFile
      putStrLn $ "(inputType, outputType, answer, ast) = " ++ show out
    _       -> print "One argument (file name) expected"
