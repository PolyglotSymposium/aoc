{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ProgramProblem
       ( runProgramProblem
       ) where

import           Builtins (programContext)
import           Data.Foldable (for_)
import           Data.Text hiding (foldr)
import qualified ProgramAst as Ast
import qualified ProgramParser as Parse
import qualified System.FilePath as Path
import           Text.Megaparsec
import           Text.Megaparsec.Error (parseErrorPretty)
import qualified Type
import qualified TypeCheck
import           Value (insert, Context)
import qualified Value as V

contextWith :: Ast.InstructionSpec -> Context
contextWith (Ast.InstParts {..}) =
  foldr insertNumeric programContext terms

  where
    insertNumeric (Ast.Number reg) context = insert reg (Type.Number, V.I 0) context
    insertNumeric (Ast.Register reg) context = insert reg (Type.Number, V.I 0) context
    insertNumeric (Ast.Literal _) context = context

runProgramProblem :: (String, String) -> IO (Maybe (Type.Type, Type.Type, V.Value, Ast.Problem))
runProgramProblem (source, text) =
  case runParser Parse.programSpec source $ pack text of
    Left err -> do
      putStrLn "Error parsing aoc code file:"
      putStrLn $ parseErrorPretty err
      pure Nothing
    Right programSpec -> do
      let solution = Ast.solution programSpec
      let context = programContext
      let programSourcePath = Path.takeDirectory source Path.</> unpack (Ast.programAt programSpec)

      programSource <- readFile programSourcePath

      case runParser (Parse.program programSpec) programSourcePath $ pack programSource of
        Left err -> do
          putStrLn "Error parsing program:"
          putStrLn $ parseErrorPretty err
          pure Nothing
        Right programAst ->
          let
            validations = do
              TypeCheck.ensureOneFreeOrIdentInEachStep context solution
              ot <- TypeCheck.unifySolution context solution Type.Program
              for_ (Ast.instructions programSpec) $ \instruction -> do
                let contextWithInstruction = contextWith instruction
                _ <- case Ast.condition instruction of
                  Just cond -> do
                    _ <- TypeCheck.noFrees contextWithInstruction cond
                    _ <- TypeCheck.unify contextWithInstruction cond Type.Boolean Nothing
                    pure ()
                  _ -> pure ()
                case Ast.meaning instruction of
                  Ast.SetRegister _ value -> do
                    _ <- TypeCheck.noFrees contextWithInstruction value
                    _ <- TypeCheck.unify contextWithInstruction value Type.Number Nothing
                    pure ()
                  _ -> pure ()
              pure ot
          in do
            print validations
            print programSpec
            print programAst
            error "TODO"
