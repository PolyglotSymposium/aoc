{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ListParser
       ( list
       , listInput
       ) where

import           Data.Maybe (fromMaybe)
import           Data.Text
import           Data.Void
import qualified ListAst as List
import qualified Parser as P
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr
import qualified Value as V

listInput :: Text -> P.Parser V.Value -> P.Parser [V.Value]
listInput sep pValue = P.ws *> sepBy pValue (string sep) <* eof

list :: P.Parser List.Problem
list = do
  _ <- P.lexeme $ P.ws *> string "list"
  _ <- P.lexeme $ string "at"
  file <- P.lexeme $ takeWhile1P (Just "path character") (\c -> not (c `elem` [' ', '\t', '\n', '\r']))
  _ <- P.lexeme $ string "separated"
  _ <- P.lexeme $ string "by"
  separator <- P.lexeme $ pack <$> (char '\'' >> manyTill L.charLiteral (char '\''))
  _ <- P.lexeme $ string "solution"
  cde <- P.lexeme code
  eof
  pure $
    List.ListProblem {
    List.at          = file
    , List.separator = separator
    , List.solution  = cde
    }

code :: P.Parser List.Solution
code = P.lexeme $ makeExprParser solutionTerm [[pipe]]

pipe :: Operator P.Parser List.Solution
pipe = InfixL (List.Pipe <$ P.lstr "|")

solutionTerm :: P.Parser List.Solution
solutionTerm =
  P.lexeme (for <|> floatingLambda)

for :: P.Parser List.Solution
for = do
  _       <- P.lexeme (string "for")
  cond    <- lambda
  gen     <- lambda
  reduce  <- optional lambda
  pure $ List.For cond gen $ fromMaybe gen reduce

lambda :: P.Parser List.Lambda
lambda = P.lexeme $ List.Body <$> value

value :: P.Parser List.Value
value = P.lexeme $ makeExprParser valueTerm [
    [InfixL (List.And <$ P.lstr "&&")]
  , [InfixL (List.Gt <$ P.lstr ">")]
  , [InfixL (List.Divide <$ P.lstr "/")]
  , [InfixL (List.Subtract <$ P.lstr "-")]
  ]

valueTerm :: P.Parser List.Value
valueTerm =
  P.lexeme (between (char '(') (char ')') value <|> List.Identifier <$> P.ident <|> List.Inte <$> L.decimal)

floatingLambda :: P.Parser List.Solution
floatingLambda = P.lexeme $ List.FloatingLambda <$> lambda
