module ListProblem
       ( runListProblem
       ) where

import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Error (parseErrorPretty)
import qualified ListAst as Ast
import qualified ListParser as Parse
import qualified ListType as ListType
import qualified Type as Type
import qualified Value as V
import qualified System.FilePath as Path
import Debug.Trace

getInputParser :: Type.Type -> Maybe (Parse.Parser V.Value)
getInputParser Type.Number = Just Parse.integer
getInputParser _ = Nothing

runListProblem :: String -> IO ()
runListProblem source = do
  text <- readFile source
  case runParser Parse.list source $ pack text of
    Left err -> do
      putStrLn "Error parsing aoc code file:"
      putStrLn $ parseErrorPretty err
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
          Left err -> print err
          Right (inputElementType, outputType) ->
            case getInputParser inputElementType of
              Nothing -> putStrLn $ "Inferred input to have element type " ++ show inputElementType ++ " (only list of integers are supported)"
              Just parseInput -> do
                inputText <- readFile inputPath
                case runParser (Parse.listInput (Ast.separator ast) parseInput) inputPath $ strip $ pack inputText of
                  Left err -> do
                    putStrLn "Error parsing input file:"
                    putStrLn $ parseErrorPretty err
                  Right input -> do
                    print input
--                print $ Type.List inputElementType
--                print outputType
--                print ast
