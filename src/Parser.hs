{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser
       ( Parser(..)
       , ws
       , integer
       , ident
       , lstr
       , lexeme
       ) where

import           Data.Maybe (fromMaybe)
import           Data.Text
import           Data.Void
import qualified ListAst as List
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr
import qualified Value as V

type Parser = Parsec Void Text

ws :: Parser ()
ws = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

integer :: Parser V.Value
integer = V.I <$> L.signed ws L.decimal

lstr :: Text -> Parser Text
lstr = lexeme . string

ident :: Parser Text
ident = lexeme $ takeWhile1P (Just "identifier") (\c -> c `elem` ("_*" ++ ['a'..'z'] ++ ['A'..'Z']))
