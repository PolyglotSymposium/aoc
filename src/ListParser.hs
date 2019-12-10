{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ListParser
       ( list
       , listInput
       , integer
       , Parser
       ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import qualified ListAst as List
import qualified Value as V
import Data.Text
import Data.Void
import Control.Monad (void)

type Parser = Parsec Void Text

ws :: Parser ()
ws = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

listInput :: Text -> Parser V.Value -> Parser [V.Value]
listInput sep pValue = ws *> sepBy pValue (string sep) <* ws <* eof

integer :: Parser V.Value
integer = V.I <$> L.signed ws L.decimal

list :: Parser List.Problem
list = do
  lexeme $ ws *> string "list"
  lexeme $ string "at"
  file <- lexeme $ takeWhile1P (Just "path character") (\c -> not (c `elem` [' ', '\t', '\n', '\r']))
  lexeme $ string "separated"
  lexeme $ string "by"
  separator <- lexeme $ pack <$> (char '\'' >> manyTill L.charLiteral (char '\''))
  lexeme $ string "solution"
  cde <- lexeme code
  eof
  pure $
    List.ListProblem {
    List.at          = file
    , List.separator = separator
    , List.solution  = cde
    }

code :: Parser List.Solution
code = lexeme $ makeExprParser solutionTerm [[pipe]]

pipe :: Operator Parser List.Solution
pipe = InfixL (List.Pipe <$ lstr "|")

solutionTerm :: Parser List.Solution
solutionTerm =
  lexeme (for <|> floatingLambda)

for :: Parser List.Solution
for = lexeme (string "for") *> (List.For <$> lambda <*> lambda <*> optional lambda)

lambda :: Parser List.Lambda
lambda = lexeme $ List.Body <$> value

lstr :: Text -> Parser Text
lstr = lexeme . string

value :: Parser List.Value
value = lexeme $ makeExprParser valueTerm [
    [InfixL (List.Gt <$ lstr ">")]
  , [InfixL (List.Divide <$ lstr "/")]
  , [InfixL (List.Subtract <$ lstr "-")]
  ]

valueTerm :: Parser List.Value
valueTerm =
  lexeme (between (char '(') (char ')') value <|> List.Identifier <$> ident <|> List.Inte <$> L.decimal)

floatingLambda :: Parser List.Solution
floatingLambda = lexeme $ List.FloatingLambda <$> lambda

ident :: Parser Text
ident = lexeme $ takeWhile1P (Just "identifier") (\c -> c `elem` ("_*" ++ ['a'..'z'] ++ ['A'..'Z']))
