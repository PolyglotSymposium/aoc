module ListProblem
       ( runListProblem
       ) where

import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Error (parseErrorPretty)
import qualified ListAst as Ast
import qualified ListEvaluator as Eval
import qualified ListParser as Parse
import qualified ListType as ListType
import qualified System.FilePath as Path
import qualified Type as Type
import qualified Value as V

getInputParser :: Type.Type -> Maybe (Parse.Parser V.Value)
getInputParser Type.Number = Just Parse.integer
getInputParser _ = Nothing

runListProblem :: String -> IO (Maybe (Type.Type, Type.Type, V.Value, Ast.Problem))
runListProblem source = do
  text <- readFile source
  case runParser Parse.list source $ pack text of
    Left err -> do
      putStrLn "Error parsing aoc code file:"
      putStrLn $ parseErrorPretty err
      pure Nothing
    Right ast ->
      let
        inputPath = Path.takeDirectory source Path.</> (unpack (Ast.at ast))
        validations = do
          _ <- ListType.ensureOneFreeOrIdentInEachStep $ Ast.solution ast
          it <- ListType.inferInputType $ Ast.solution ast
          -- TODO, needed?
          _ <- ListType.inferOutputType $ Ast.solution ast
          ot <- ListType.unifySolution (Ast.solution ast) (Type.List it)
          pure $ (it, ot)
      in
        case validations of
          Left err -> do
            print err
            pure Nothing
          Right (inputElementType, outputType) ->
            case getInputParser inputElementType of
              Nothing -> do
                putStrLn $ "Inferred input to have element type " ++ show inputElementType ++ " (only list of integers are supported)"
                pure Nothing
              Just parseInput -> do
                inputText <- readFile inputPath
                case runParser (Parse.listInput (Ast.separator ast) parseInput) inputPath $ strip $ pack inputText of
                  Left err -> do
                    putStrLn "Error parsing input file:"
                    putStrLn $ parseErrorPretty err
                    pure Nothing
                  Right input -> do
                    case Eval.eval (V.Vs input) (Ast.solution ast) of
                      Right result -> do
                        print result
                        pure $ Just (Type.List inputElementType, outputType, result, ast)
                      Left err -> do
                        print err
                        pure Nothing
