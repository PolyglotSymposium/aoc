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

getInputParser :: Type.Type -> Maybe (Parse.Parser V.Value)
getInputParser Type.Number = Just Parse.integer
getInputParser _ = Nothing

runListProblem :: String -> IO ()
runListProblem source = do
  text <- readFile source
  case runParser Parse.list source $ pack text of
    Left err -> putStrLn $ parseErrorPretty err
    Right ast ->
      case ListType.inferInputType $ Ast.solution ast of
        Left err -> print err
        Right inputType ->
          case getInputParser inputType of
            Nothing -> putStrLn $ "Inferred input to have type " ++ show inputType ++ " (only list of integers are supported)"
            Just parseInput -> do
              print inputType
              print ast
