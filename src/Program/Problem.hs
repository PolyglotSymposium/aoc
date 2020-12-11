{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Program.Problem
       ( runProgramProblem
       ) where

import           Builtins (programContext)
import           Data.Foldable (for_)
import           Data.Functor (($>))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Text hiding (foldr)
import qualified Evaluator as Eval
import qualified Program.Ast as Ast
import qualified Program.Parser as Parse
import qualified System.FilePath as Path
import           Text.Megaparsec
import           Text.Megaparsec.Error (parseErrorPretty)
import qualified Type
import qualified TypeCheck
import           Value (insert, Context)
import qualified Value as V

withRegisters :: S.Set Text -> Ast.IntermediateProgram -> Context -> Context
withRegisters globalRegisters prog context =
  foldr addToContext context $ (S.toList globalRegisters ++) $ Ast.allRegisters prog

  where
    addToContext reg = insert reg (Type.Register, V.Register reg)

contextWith :: S.Set Text -> Ast.InstructionSpec -> Context
contextWith globals Ast.InstParts{..} =
  foldr insertReg (foldr insertNumeric programContext terms) globals

  where
    insertReg = (`insert ` (Type.Number, V.I 0))

    insertNumeric (Ast.Number reg) context = insert reg (Type.Number, V.I 0) context
    insertNumeric (Ast.Register reg) context = insert reg (Type.Number, V.I 0) context
    insertNumeric (Ast.Val reg) context = insert reg (Type.Number, V.I 0) context
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
            executionContext = withRegisters (Ast.globalRegisters programSpec) programAst context
            program =
              V.Program
                (Ast.indexed $ Ast.inlineSpecs (Ast.globalRegisters programSpec) programAst)
                (V.Ip 0)
                (V.registersFrom $ (, Ast.initialRegisterValue programSpec) <$> (S.toList (Ast.globalRegisters programSpec) ++ Ast.allRegisters programAst))
                (V.Traces
                 {
                   V.registerValues =
                     if S.member Ast.TraceRegisterValues (Ast.traces programSpec)
                     then Just $ V.RegHistory M.empty
                     else Nothing
                 , V.instructionPointers = S.empty
                 }
                )
            validations = do
              TypeCheck.ensureOneFreeOrIdentInEachStep executionContext solution
              ot <- TypeCheck.unifySolution executionContext solution Type.Program
              for_ (Ast.instructions programSpec) $ \instruction -> do
                let contextWithInstruction = contextWith (Ast.globalRegisters programSpec) instruction
                _ <- case Ast.condition instruction of
                  Just cond ->
                    TypeCheck.noFrees contextWithInstruction cond *>
                    TypeCheck.unify contextWithInstruction cond Type.Boolean Nothing $> ()
                  _ -> pure ()
                case Ast.meaning instruction of
                  Ast.SetRegister _ value ->
                    TypeCheck.noFrees contextWithInstruction value *>
                    TypeCheck.unify contextWithInstruction value Type.Number Nothing $> ()
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
                    print executionContext
                    print err
                    pure Nothing
