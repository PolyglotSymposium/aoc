module Passwords.Problem
       ( runPasswordsProblem
       ) where

import Prelude hiding (lines)

import           Builtins (passwordsContext)
import           Data.Text hiding (foldr)
import qualified Evaluator as Eval
import qualified Passwords.Ast as Ast
import qualified Passwords.Parser as Parse
import qualified System.FilePath as Path
import           Text.Megaparsec
import           Text.Megaparsec.Error (parseErrorPretty)
import qualified Type
import qualified TypeCheck
import qualified Value as V

runPasswordsProblem :: (String, String) -> IO (Maybe (Type.Type, Type.Type, V.Value, Ast.Problem))
runPasswordsProblem (source, text) =
  case runParser Parse.problem source $ pack text of
    Left err -> do
      putStrLn "Error parsing aoc code file:"
      putStrLn $ parseErrorPretty err
      pure Nothing
    Right ast ->
      let
        inputPath = Path.takeDirectory source Path.</> unpack (Ast.at ast)
        solution = Ast.solution ast
        validations = do
          TypeCheck.ensureOneFreeOrIdentInEachStep passwordsContext solution
          ot <- TypeCheck.unifySolution passwordsContext solution (Type.List Type.Text)
          pure ot
      in
      case validations of
        Left err -> do
          print err
          pure Nothing
        Right outputType -> do
          inputText <- readFile inputPath
          let input = V.Vs $ fmap V.Txt $ lines $ pack inputText
          case Eval.eval (V.insert (pack "$validityRules") (Type.Hidden, V.ValidityRules $ Ast.validityRules ast) passwordsContext) input (Ast.solution ast) of
            Right result -> do
              print result
              pure $ Just (Type.List Type.Text, outputType, result, ast)
            Left err -> do
              print err
              pure Nothing
