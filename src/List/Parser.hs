{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module List.Parser
       ( list
       , listInput
       ) where

import           Data.Text
import qualified List.Ast as List
import qualified Parser as P
import           Text.Megaparsec
import qualified Value as V

listInput :: Text -> P.Parser V.Value -> P.Parser [V.Value]
listInput sep pValue = P.ws *> sepBy pValue (P.lstr sep) <* eof

list :: P.Parser List.Problem
list = do
  _ <- P.ws *> P.lstr "list" *> P.lstr "at"
  file <- P.filePath
  _ <- P.lstr "separated" *> P.lstr "by"
  separator <- P.simpleQuoted
  _ <- P.lstr "solution"
  code <- P.code
  eof
  pure $
    List.ListProblem {
      List.at        = file
    , List.separator = separator
    , List.solution  = code
    }
