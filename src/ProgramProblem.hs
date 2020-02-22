{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module ProgramProblem
       ( runProgramProblem
       ) where

import           Builtins (programContext)
import           Data.Foldable (for_)
import           Data.Text hiding (foldr)
import qualified ListEvaluator as Eval
import qualified ProgramAst as Ast
import qualified ProgramParser as Parse
import qualified System.FilePath as Path
import           Text.Megaparsec
import           Text.Megaparsec.Error (parseErrorPretty)
import qualified Type
import qualified TypeCheck
import           Value (insert, Context)
import qualified Value as V

withRegisters :: Ast.IntermediateProgram -> Context -> Context
withRegisters prog context =
  foldr addToContext context $ Ast.allRegisters prog

  where
    addToContext reg = insert reg (Type.Register, V.Register reg)

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
      let programSourcePath = Path.takeDirectory source Path.</> unpack (Ast.programAt programSpec)

      programSource <- readFile programSourcePath

      case runParser (Parse.program programSpec) programSourcePath $ pack programSource of
        Left err -> do
          putStrLn "Error parsing program:"
          putStrLn $ parseErrorPretty err
          pure Nothing
        Right programAst ->
          let
            solution = Ast.solution programSpec
            context = programContext
            executionContext = withRegisters programAst context
            program =
              V.Program (Ast.indexed $ Ast.inlineSpecs programAst) (V.Ip 0) $ V.registersFrom $ (, Ast.initialRegisterValue programSpec) <$> Ast.allRegisters programAst
            validations = do
              TypeCheck.ensureOneFreeOrIdentInEachStep executionContext solution
              ot <- TypeCheck.unifySolution executionContext solution Type.Program
              for_ (Ast.instructions programSpec) $ \instruction -> do
                let contextWithInstruction = contextWith instruction
                _ <- case Ast.condition instruction of
                  Just cond ->
                    TypeCheck.noFrees contextWithInstruction cond *>
                    TypeCheck.unify contextWithInstruction cond Type.Boolean Nothing *>
                    pure ()
                  _ -> pure ()
                case Ast.meaning instruction of
                  Ast.SetRegister _ value ->
                    TypeCheck.noFrees contextWithInstruction value *>
                    TypeCheck.unify contextWithInstruction value Type.Number Nothing *>
                    pure ()
                  _ -> pure ()
              pure ot
          in
            case validations of
              Left err -> do
                print err
                pure Nothing
              Right outputType ->
                case Eval.eval executionContext program solution of
                  Right result -> do
                    print result
                    pure $ Just (Type.Program, outputType, result, programSpec)
                  Left err -> do
                    print program
                    print executionContext
                    print err
                    pure Nothing
