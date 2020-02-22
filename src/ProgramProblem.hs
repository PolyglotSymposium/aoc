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
import           Text.Megaparsec
import           Text.Megaparsec.Error (parseErrorPretty)
import qualified Type
import qualified TypeCheck
import           Value (add, insert, Context)
import qualified Value as V

contextWith :: Ast.Instruction -> Context
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
    Right ast ->
      let
        solution = Ast.solution ast
        context = programContext

        validations = do
          TypeCheck.ensureOneFreeOrIdentInEachStep context solution
          ot <- TypeCheck.unifySolution context solution Type.Program
          for_ (Ast.instructions ast) $ \instruction -> do
            let contextWithInstruction = contextWith instruction
            _ <- case Ast.condition instruction of
              Just cond -> do
                TypeCheck.noFrees contextWithInstruction cond
                TypeCheck.unify contextWithInstruction cond Type.Boolean Nothing
                pure ()
              _ -> pure ()
            case Ast.meaning instruction of
              Ast.SetRegister _ value -> do
                TypeCheck.noFrees contextWithInstruction value
                TypeCheck.unify contextWithInstruction value Type.Number Nothing
                pure ()
              _ -> pure ()
          pure ot
      in do
        print validations
        error "TODO"
