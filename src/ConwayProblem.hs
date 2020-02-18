{-# LANGUAGE OverloadedStrings #-}

module ConwayProblem
       ( runConwayProblem
       ) where

import qualified Ast as A
import           Builtins (conwayContext)
import qualified ConwayAst as Ast
import qualified ConwayParser as Parse
import           Data.Foldable (for_)
import qualified Data.Map.Strict as M
import           Data.Text
import qualified ListEvaluator as Eval
import           System.Console.ANSI
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

solve :: Context -> V.Value -> Type.Type -> Ast.Problem -> IO (Maybe (Type.Type, Type.Type, V.Value, Ast.Problem))
solve context initialState outputType ast =
  case Ast.solution ast of
    Ast.Solution solution ->
      case Eval.eval context initialState solution of
        Right result -> do
          print result
          pure $ Just (Type.Grid, outputType, result, ast)
        Left err -> do
          print err
          pure Nothing

    Ast.Animate Ast.Forever -> animateForever initialState
      where
        animateForever state =
          case Eval.eval context state (solutionForDirective ast) of
            Right nextGeneration -> do
              clearScreen
              print nextGeneration
              animateForever nextGeneration
            Left err -> do
              print err
              pure Nothing

solutionForDirective :: Ast.Problem -> A.Solution
solutionForDirective ast =
  case Ast.solution ast of
    Ast.Solution sol -> sol
    Ast.Animate _ -> Ast.nextGenerationSolution

runConwayProblem :: (String, String) -> IO (Maybe (Type.Type, Type.Type, V.Value, Ast.Problem))
runConwayProblem (source, text) =
  case runParser Parse.conway source $ pack text of
    Left err -> do
      putStrLn "Error parsing aoc code file:"
      putStrLn $ parseErrorPretty err
      pure Nothing
    Right ast ->
      let
        context = addAliasesToContext (Ast.cellAliases ast) conwayContext
        solution = solutionForDirective ast

        validations = do
          TypeCheck.ensureOneFreeOrIdentInEachStep context solution
          ot <- TypeCheck.unifySolution context solution Type.Grid
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
            let parseState = case Ast.dimensions ast of
                               Ast.OneD -> Parse.oneDimensionalConwayInput
                               Ast.TwoD -> Parse.twoDimensionalConwayInput
            case runParser (parseState (Ast.cellTransitions ast) (Ast.cellAliases ast)) stateSource $ pack initialStateText of
              Left err -> do
                putStrLn "Error parsing Conway initial state:"
                putStrLn $ parseErrorPretty err
                pure Nothing

              Right initialState ->
                let
                  context' =
                    insert "$generation_0" (Type.Grid, initialState) $
                      case Ast.outOfBoundsCellsAre ast of
                        Just (Ast.CellIdent c) -> insert "$oob" (Type.CellState, V.CellState c) context
                        _                      -> context
                in
                  solve context' initialState outputType ast
