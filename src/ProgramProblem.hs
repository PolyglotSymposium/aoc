{-# LANGUAGE OverloadedStrings #-}

module ProgramProblem
       ( runProgramProblem
       ) where

import qualified Type
import qualified Value as V

runProgramProblem :: (String, String) -> IO (Maybe (Type.Type, Type.Type, V.Value, ()))
runProgramProblem (source, text) = undefined
