module Main (main) where

import qualified Aoc
import qualified System.Environment as Env

main :: IO ()
main = do
  arg <- Env.getArgs
  case arg of
    [inputFile] ->
      () <$ Aoc.solve inputFile
    [inputFile, "--more-info"] -> do
      out <- Aoc.solve inputFile
      case out of
        Just (it, ot, answer, Aoc.List ast) -> do
          putStrLn "List domain:"
          putStrLn $ "(inputType, outputType, answer, ast) = " ++ show (it, ot, answer, ast)
        Just (it, ot, answer, Aoc.Conway ast) -> do
          putStrLn "Conway domain:"
          putStrLn $ "(inputType, outputType, answer, ast) = " ++ show (it, ot, answer, ast)
        Just (it, ot, answer, Aoc.Program ast) -> do
          putStrLn "Program domain:"
          putStrLn $ "(inputType, outputType, answer, ast) = " ++ show (it, ot, answer, ast)
        Just (it, ot, answer, Aoc.Turtle ast) -> do
          putStrLn "Turtle domain:"
          putStrLn $ "(inputType, outputType, answer, ast) = " ++ show (it, ot, answer, ast)
        Just (it, ot, answer, Aoc.Graph ast) -> do
          putStrLn "Graph domain:"
          putStrLn $ "(inputType, outputType, answer, ast) = " ++ show (it, ot, answer, ast)
        Just (it, ot, answer, Aoc.Passwords ast) -> do
          putStrLn "Passwords domain:"
          putStrLn $ "(inputType, outputType, answer, ast) = " ++ show (it, ot, answer, ast)
        Nothing -> pure ()
    _       -> print "One argument (file name) expected"
