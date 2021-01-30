module List.Problem
       ( runListProblem
       ) where

import           Builtins (listContext, numberFromParsedLine)
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

getInputParser :: [Ast.ParseTerm] -> (Parse.Parser V.Value, Type.Type)
getInputParser [] = (Parse.integer, Type.Number)
getInputParser terms = (Parse.parsedLine terms, Type.ParsedLine)

listContextWithParseTerms :: [Ast.ParseTerm] -> V.Context
listContextWithParseTerms = foldr extend listContext
  where
    extend (Ast.Literal _) ctxt = ctxt
    extend (Ast.Number name) ctxt =  V.insert name (Type.ParsedLine `Type.Arrow` Type.Number, numberFromParsedLine name) ctxt

runListProblem :: (String, String) -> IO (Maybe (Type.Type, Type.Type, V.Value, Ast.Problem))
runListProblem (source, text) =
  case runParser Parse.list source $ pack text of
    Left err -> do
      putStrLn "Error parsing aoc code file:"
      putStrLn $ parseErrorPretty err
      pure Nothing
    Right ast ->
      let
        inputPath = Path.takeDirectory source Path.</> unpack (Ast.at ast)
        context = listContextWithParseTerms $ Ast.parseTerms ast
        validations = do
          _ <- TypeCheck.ensureOneFreeOrIdentInEachStep context $ Ast.solution ast
          it <- TypeCheck.inferInputType context $ Ast.solution ast
          ot <- TypeCheck.unifySolution context (Ast.solution ast) (Type.List it)
          pure (it, ot)
      in
        case validations of
          Left err -> do
            print err
            pure Nothing
          Right (_, outputType) ->
            let
              (parseInput, inputElementType) = getInputParser $ Ast.parseTerms ast
            in do
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
