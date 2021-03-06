{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser
       ( Parser
       , ws
       , integer
       , rawInteger
       , ident
       , lstr
       , phrase
       , phraseAs
       , lexeme
       , filePath
       , simpleQuoted
       , code
       , value
       , textLiteral 
       ) where

import qualified Ast
import           Data.Foldable (traverse_)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Text
import           Data.List.NonEmpty as NE
import           Data.Tuple (swap)
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

phrase :: [Text] -> Parser ()
phrase = traverse_ lstr

phraseAs :: [Text] -> a -> Parser a
phraseAs v r = pure r <* phrase v

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
    [InfixL (Ast.Xor <$ lstr ".^.")]
  , [InfixL (Ast.Equals <$ lstr "="), InfixL (Ast.NotEquals <$ lstr "/=")]
  , [InfixL (Ast.Geq <$ lstr ">="), InfixL (Ast.Leq <$ lstr "<="), InfixL (Ast.Gt <$ lstr ">"), InfixL (Ast.Lt <$ lstr "<")]
  , [InfixL (Ast.And <$ lstr "&&"), InfixL (Ast.Or <$ lstr "||")]
  , [InfixL (Ast.Raised <$ lstr "^")]
  , [InfixL (Ast.Multiply <$ lstr "*"), InfixL (Ast.Divide <$ lstr "/")]
  , [InfixL (Ast.Add    <$ lstr "+")]
  , [InfixL (Ast.Subtract <$ lstr "-")]
  , [InfixR (Ast.FlipCompose <$ lstr "@")]
  ]

textLiteral :: Parser Text
textLiteral =
  pack <$> between (char '"') (char '"') (some (satisfy (/= '"')))

valueTerm :: Parser Ast.Value
valueTerm =
  lexeme (between (char '(') (char ')') value
          <|> try applicationOrIdent
          <|> Ast.List <$> between (char '[') (char ']') (sepBy value $ lexeme $ char ',')
          <|> Ast.Pos <$> positionLiteral
          <|> Ast.Text <$> textLiteral
          <|> Ast.Inte <$> L.decimal
         )
  where
    applicationOrIdent = do
      ident' <- ident
      args <- many (try $ lexeme value)
      pure $ case args of
        [] -> Ast.Identifier ident'
        (v:vs) -> Ast.Application ident' $ v NE.:| vs

positionLiteral :: Parser (Integer, Integer)
positionLiteral =
  between (lstr "{") (lstr "}") (keys "x" "y" <|> (swap <$> keys "y" "x"))

  where
    keys a b = do
      a' <- key a
      _ <- lstr ","
      b' <- key b
      pure (a', b')

    key a = lstr a *> lstr "=" *> lexeme rawInteger

floatingLambda :: Parser Ast.Solution
floatingLambda = lexeme $ Ast.FloatingLambda <$> lambda
