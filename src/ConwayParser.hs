{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConwayParser
       ( Parser
       ) where

import Data.Maybe (fromMaybe)
import Data.Text
import Data.Void
import qualified ListAst as List
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import qualified Value as V

type Parser = Parsec Void Text

ws :: Parser ()
ws = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

listInput :: Text -> Parser V.Value -> Parser [V.Value]
listInput sep pValue = ws *> sepBy pValue (string sep) <* eof

integer :: Parser V.Value
integer = V.I <$> L.signed ws L.decimal

list :: Parser List.Problem
list = do
  _ <- lexeme $ ws *> string "list"
  _ <- lexeme $ string "at"
  file <- lexeme $ takeWhile1P (Just "path character") (\c -> not (c `elem` [' ', '\t', '\n', '\r']))
  _ <- lexeme $ string "separated"
  _ <- lexeme $ string "by"
  separator <- lexeme $ pack <$> (char '\'' >> manyTill L.charLiteral (char '\''))
  _ <- lexeme $ string "solution"
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
for = do
  _       <- lexeme (string "for")
  cond    <- lambda
  gen     <- lambda
  reduce  <- optional lambda
  pure $ List.For cond gen $ fromMaybe gen reduce

lambda :: Parser List.Lambda
lambda = lexeme $ List.Body <$> value

lstr :: Text -> Parser Text
lstr = lexeme . string

value :: Parser List.Value
value = lexeme $ makeExprParser valueTerm [
    [InfixL (List.And <$ lstr "&&")]
  , [InfixL (List.Gt <$ lstr ">")]
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