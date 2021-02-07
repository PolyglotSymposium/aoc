module Graph.Problem
       ( runGraphProblem
       ) where

import           Builtins (graphContext)
import           Data.Text hiding (foldr)
import qualified Evaluator as Eval
import qualified Graph.Ast as Ast
import qualified Graph.Parser as Parse
import qualified Parser as Parse
import qualified System.FilePath as Path
import           Text.Megaparsec
import           Text.Megaparsec.Error (parseErrorPretty)
import qualified Type
import qualified TypeCheck
import qualified Value as V

runGraphProblem :: (String, String) -> IO (Maybe (Type.Type, Type.Type, V.Value, Ast.Problem))
runGraphProblem (source, text) =
  case runParser Parse.list source $ pack text of
    Left err -> do
      putStrLn "Error parsing aoc code file:"
      putStrLn $ parseErrorPretty err
      pure Nothing
    Right ast ->
      let
        inputPath = Path.takeDirectory source Path.</> unpack (Ast.at ast)
        solution = Ast.solution ast
        validations = do
          TypeCheck.ensureOneFreeOrIdentInEachStep graphContext solution
          ot <- TypeCheck.unifySolution graphContext solution Type.Graph
          pure ot
      in
      case validations of
        Left err -> do
          print err
          pure Nothing
        Right outputType -> do
          inputText <- readFile inputPath
          case Parse.inputGraph ast $ pack inputText of
            Left err -> do
              putStrLn "Error parsing input file:"
              putStrLn $ parseErrorPretty err
              pure Nothing
            Right input ->
              case Eval.eval graphContext input (Ast.solution ast) of
                Right result -> do
                  print result
                  pure $ Just (Type.Graph, outputType, result, ast)
                Left err -> do
                  print err
                  pure Nothing
