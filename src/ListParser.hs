{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ListParser
       ( list
       ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import qualified ListAst as List
import Data.Text
import Data.Void

type Parser = Parsec Void Text

ws :: Parser ()
ws = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme = L.lexeme ws

list :: Parser List.Problem
list = do
  ws
  lexeme $ string "list"
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
pipe = InfixL (List.Pipe <$ char '|')

solutionTerm :: Parser List.Solution
solutionTerm =
  lexeme (for <|> floatingLambda)

for :: Parser List.Solution
for = lexeme (string "for") *> (List.For <$> lambda <*> lambda <*> optional lambda)

lambda :: Parser List.Lambda
lambda = lexeme $ List.Body <$> value

value :: Parser List.Value
value = lexeme $ makeExprParser valueTerm [
    [InfixL (List.Gt <$ char '<')]
  , [InfixL (List.Divide <$ char '/')]
  , [InfixL (List.Subtract <$ char '-')]
  ]

valueTerm :: Parser List.Value
valueTerm =
  lexeme (List.Identifier <$> ident <|> List.Inte <$> L.decimal)

floatingLambda :: Parser List.Solution
floatingLambda = lexeme $ List.FloatingLambda <$> lambda

ident :: Parser Text
ident = lexeme $ takeWhile1P (Just "identifier") (\c -> c `elem` ('_':['a'..'z'] ++ ['A'..'Z']))
