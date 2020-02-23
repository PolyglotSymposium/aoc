{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser
       ( Parser
       , ws
       , integer
       , rawInteger
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
import qualified Data.Set as S
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
integer = V.I <$> rawInteger

rawInteger :: Parser Integer
rawInteger = L.signed ws L.decimal

lstr :: Text -> Parser Text
lstr = lexeme . string

identStart :: S.Set Char
identStart = S.fromList ("_$" ++ ['a'..'z'] ++ ['A'..'Z'])

identRest :: S.Set Char
identRest = S.union identStart $ S.fromList ("*" ++ ['0'..'9'])

ident :: Parser Text
ident = lexeme $ do
  start <- takeWhile1P (Just "identifier start") (`S.member` identStart)
  rest <- takeWhileP (Just "identifier end") (`S.member` identRest)
  pure $ start <> rest

filePath :: Parser Text
filePath = lexeme $ takeWhile1P (Just "path character") (`notElem` [' ', '\t', '\n', '\r'])

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
    [InfixL (Ast.Equals <$ lstr "="), InfixL (Ast.NotEquals <$ lstr "/=")]
  , [InfixL (Ast.Geq <$ lstr ">="), InfixL (Ast.Gt <$ lstr ">")]
  , [InfixL (Ast.And <$ lstr "&&"), InfixL (Ast.Or <$ lstr "||")]
  , [InfixL (Ast.Raised <$ lstr "^")]
  , [InfixL (Ast.Multiply <$ lstr "*"), InfixL (Ast.Divide <$ lstr "/")]
  , [InfixL (Ast.Add    <$ lstr "+")]
  , [InfixL (Ast.Subtract <$ lstr "-")]
  ]

valueTerm :: Parser Ast.Value
valueTerm =
  lexeme (between (char '(') (char ')') value
          <|> try application
          <|> Ast.Identifier <$> ident
          <|> Ast.List <$> between (char '[') (char ']') (sepBy value $ lexeme $ char ',')
          <|> Ast.Inte <$> L.decimal
         )

application :: Parser Ast.Value
application = Ast.Application <$> lexeme ident <*> lexeme value

floatingLambda :: Parser Ast.Solution
floatingLambda = lexeme $ Ast.FloatingLambda <$> lambda
