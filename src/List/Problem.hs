module List.Problem
       ( runListProblem
       ) where

import           Builtins (listContext, valueFromParsedLine)
import           Data.Text hiding (foldr)
import qualified Evaluator as Eval
import qualified List.Ast as Ast
import qualified List.Parser as Parse
import qualified Parser as Parse
import qualified System.FilePath as Path
import           Text.Megaparsec
import           Text.Megaparsec.Error (parseErrorPretty)
import qualified Type
import qualified TypeCheck
import qualified Value as V

getInputParser :: Text -> [Ast.ParseTerm] -> (Parse.Parser V.Value, Type.Type)
getInputParser _ [] = (Parse.integer, Type.Number)
getInputParser sep terms = (Parse.parsedLine sep terms, Type.ParsedLine)

listContextWithParseTerms :: [Ast.ParseTerm] -> V.Context
listContextWithParseTerms = foldr extend listContext
  where
    extend (Ast.Literal _) = id
    extend (Ast.Number name) = addLookup name Type.Number
    extend (Ast.Char name) = addLookup name Type.Char
    extend (Ast.Text name) = addLookup name Type.Text

    addLookup name ty = V.insert name (Type.ParsedLine `Type.Arrow` ty, valueFromParsedLine name)

runListProblem :: (String, String) -> IO (Maybe (Type.Type, Type.Type, V.Value, Ast.Problem))
runListProblem (source, text) =
  case runParser Parse.list source $ pack text of
    Left err -> do
      putStrLn "Error parsing aoc code file:"
      putStrLn $ parseErrorPretty err
      pure Nothing
    Right ast ->
      let
        (parseInput, inputElementType) = getInputParser (Ast.separator ast) $ Ast.parseTerms ast
        inputPath = Path.takeDirectory source Path.</> unpack (Ast.at ast)
        context = listContextWithParseTerms $ Ast.parseTerms ast
        validations = do
          _ <- TypeCheck.ensureOneFreeOrIdentInEachStep context $ Ast.solution ast
          ot <- TypeCheck.unifySolution context (Ast.solution ast) (Type.List inputElementType)
          pure (inputElementType, ot)
      in
        case validations of
          Left err -> do
            print err
            pure Nothing
          Right (_, outputType) -> do
            inputText <- readFile inputPath
            case runParser (Parse.listInput (Ast.separator ast) parseInput) inputPath $ strip $ pack inputText of
              Left err -> do
                putStrLn "Error parsing input file:"
                putStrLn $ parseErrorPretty err
                pure Nothing
              Right input ->
                case Eval.eval context (V.Vs input) (Ast.solution ast) of
                  Right result -> do
                    print result
                    pure $ Just (Type.List inputElementType, outputType, result, ast)
                  Left err -> do
                    print err
                    pure Nothing
