module Main (main) where

import qualified Aoc as Aoc
import qualified System.Environment as Env
import Data.Text

main :: IO ()
main = do
  arg <- Env.getArgs
  case arg of
    [inputFile] -> do
      text <- readFile inputFile
      case Aoc.parse inputFile $ pack text of
        Right ast -> print ast
        Left err -> putStrLn $ Aoc.parseErrorPretty err
    _       -> print "One argument (file name) expected"
