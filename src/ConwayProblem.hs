module ConwayProblem
       ( runConwayProblem
       ) where

import           Builtins (conwayContext, add, Context)
import qualified ConwayAst as Ast
import qualified ConwayParser as Parse
import qualified Data.Map.Strict as M
import           Data.Text
import qualified System.FilePath as Path
import           Text.Megaparsec
import           Text.Megaparsec.Error (parseErrorPretty)
import qualified Type
import qualified TypeCheck
import qualified Value as V

addAliasesToContext :: Ast.CellAliases -> Context -> Context
addAliasesToContext aliases context =
  let
    aliasContext =
      M.fromList $ fmap (\(Ast.CellIdent c, Ast.CellAlias a) -> (a, (Type.CellState, V.CellState c))) aliases
  in
    add aliasContext context

runConwayProblem :: String -> IO (Maybe (Type.Type, Type.Type, V.Value, Ast.Problem))
runConwayProblem source = do
  text <- readFile source
  putStrLn text
  case runParser Parse.conway source $ pack text of
    Left err -> do
      putStrLn "Error parsing aoc code file:"
      putStrLn $ parseErrorPretty err
      pure Nothing
    Right ast ->
      let
        inputPath = Path.takeDirectory source Path.</> unpack (Ast.initialStateAt ast)
        validations =
          case Ast.solution ast of
            Ast.Solution solution -> do
              TypeCheck.ensureOneFreeOrIdentInEachStep conwayContext solution
              it <- TypeCheck.inferInputType conwayContext solution
              ot <- TypeCheck.unifySolution conwayContext solution Type.Grid
              pure (it, ot)
      in
        do
          print inputPath
          print validations
          print ast
          undefined
--        validations = do
--          pure $ (it, ot)
--      in
--        case validations of
--          Left err -> do
--            print err
--            pure Nothing
--          Right (inputElementType, outputType) ->
--            case getInputParser inputElementType of
--              Nothing -> do
--                putStrLn $ "Inferred input to have element type " ++ show inputElementType ++ " (only list of integers are supported)"
--                pure Nothing
--              Just parseInput -> do
--                inputText <- readFile inputPath
--                case runParser (Parse.listInput (Ast.separator ast) parseInput) inputPath $ strip $ pack inputText of
--                  Left err -> do
--                    putStrLn "Error parsing input file:"
--                    putStrLn $ parseErrorPretty err
--                    pure Nothing
--                  Right input -> do
--                    case Eval.eval (V.Vs input) (Ast.solution ast) of
--                      Right result -> do
--                        print result
--                        pure $ Just (Type.List inputElementType, outputType, result, ast)
--                      Left err -> do
--                        print err
--                        pure Nothing
