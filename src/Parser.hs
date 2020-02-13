{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser
       ( Parser
       , ws
       , integer
       , ident
       , lstr
       , lexeme
       , filePath
       , simpleQuoted
       , code
       , value
       ) where

import qualified Ast
import           Data.Maybe (fromMaybe)
import           Data.Text
import           Data.Void
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

filePath :: Parser Text
filePath = lexeme $ takeWhile1P (Just "path character") (\c -> not (c `elem` [' ', '\t', '\n', '\r']))

simpleQuoted :: Parser Text
simpleQuoted = lexeme $ pack <$> (char '\'' >> manyTill L.charLiteral (char '\''))

code :: Parser Ast.Solution
code = lexeme $ makeExprParser solutionTerm [[pipe]]

pipe :: Operator Parser Ast.Solution
pipe = InfixL (Ast.Pipe <$ lstr "|")

solutionTerm :: Parser Ast.Solution
solutionTerm =
  lexeme (for <|> floatingLambda)

for :: Parser Ast.Solution
for = do
  _       <- lexeme (string "for")
  cond    <- lambda
  gen     <- lambda
  reduce  <- optional lambda
  pure $ Ast.For cond gen $ fromMaybe gen reduce

lambda :: Parser Ast.Lambda
lambda = lexeme $ Ast.Body <$> value

value :: Parser Ast.Value
value = lexeme $ makeExprParser valueTerm [
    [InfixL (Ast.Equals <$ lstr "=")]
  , [InfixL (Ast.And <$ lstr "&&"), InfixL (Ast.Or <$ lstr "||")]
  , [InfixL (Ast.Gt <$ lstr ">")]
  , [InfixL (Ast.Raised <$ lstr "^")]
  , [InfixL (Ast.Divide <$ lstr "/")]
  , [InfixL (Ast.Add <$ lstr "+")]
  , [InfixL (Ast.Subtract <$ lstr "-")]
  ]

valueTerm :: Parser Ast.Value
valueTerm =
  lexeme (between (char '(') (char ')') value
          <|> try application
          <|> Ast.Identifier <$> ident
          <|> Ast.Inte <$> L.decimal
         )

application :: Parser Ast.Value
application = Ast.Application <$> lexeme ident <*> lexeme value

floatingLambda :: Parser Ast.Solution
floatingLambda = lexeme $ Ast.FloatingLambda <$> lambda
