{-# LANGUAGE OverloadedStrings #-}

module ConwayProblem
       ( runConwayProblem
       ) where

import           Builtins (conwayContext)
import qualified ConwayAst as Ast
import qualified ConwayParser as Parse
import           Data.Foldable (for_)
import qualified Data.Map.Strict as M
import           Data.Text
import qualified ListEvaluator as Eval
import qualified System.FilePath as Path
import           Text.Megaparsec
import           Text.Megaparsec.Error (parseErrorPretty)
import qualified Type
import qualified TypeCheck
import           Value (add, insert, Context)
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
        context = addAliasesToContext (Ast.cellAliases ast) conwayContext
        solution =
          case Ast.solution ast of
            Ast.Solution solution -> solution

        validations = do
          TypeCheck.ensureOneFreeOrIdentInEachStep context solution
          ot <- TypeCheck.unifySolution conwayContext solution Type.Grid
          for_ (Ast.transitionCases $ Ast.cellTransitions ast) $ \transitionCase -> do
            TypeCheck.noFrees context transitionCase
            TypeCheck.unify context transitionCase Type.Boolean Nothing
          pure ot
      in
        case validations of
          Left err -> do
            print err
            pure Nothing
          Right outputType -> do
            let stateSource = Path.takeDirectory source Path.</> unpack (Ast.initialStateAt ast)
            initialStateText <- readFile stateSource
            case runParser (Parse.twoDimensionalConwayInput context (Ast.cellTransitions ast) (Ast.cellAliases ast)) stateSource $ pack initialStateText of
              Left err -> do
                putStrLn "Error parsing Conway initial state:"
                putStrLn $ parseErrorPretty err
                pure Nothing

              Right (width, height, initialState) ->
                let
                  contextWithSize =
                    insert "$height" (Type.Number, V.I height) $ insert "$width" (Type.Number, V.I width) context
                in
                  case Eval.eval contextWithSize initialState solution of
                    Right result -> do
                      print result
                      pure Nothing
                    Left err -> do
                      print err
                      pure Nothing
