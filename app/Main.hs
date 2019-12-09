module Main (main) where

import qualified Aoc as Aoc
import qualified System.Environment as Env

main :: IO ()
main = do
  arg <- Env.getArgs
  case arg of
    [inputFile] -> Aoc.runListProblem inputFile
    _       -> print "One argument (file name) expected"
