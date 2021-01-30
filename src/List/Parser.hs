{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module List.Parser
       ( list
       , listInput
       , parsedLine
       ) where

import           Data.Text
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, maybe, catMaybes)
import qualified List.Ast as List
import qualified Parser as P
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Value as V

listInput :: Text -> P.Parser V.Value -> P.Parser [V.Value]
listInput sep pValue = P.ws *> sepBy pValue (P.lstr sep) <* eof

list :: P.Parser List.Problem
list = do
  _ <- P.ws *> P.lstr "list" *> P.lstr "at"
  file <- P.filePath
  _ <- P.lstr "separated" *> P.lstr "by"
  separator <- P.simpleQuoted
  terms <- optional parseTerms
  _ <- maybe (P.lstr "solution") (const (pure "")) terms
  code <- P.code
  eof
  pure $
    List.ListProblem {
      List.at         = file
    , List.separator  = separator
    , List.solution   = code
    , List.parseTerms = fromMaybe [] terms
    }

parseTerms :: P.Parser [List.ParseTerm]
parseTerms = P.phrase ["where", "each", "parses", "as"] *> manyTill term (P.lstr "solution")

term :: P.Parser List.ParseTerm
term = P.lexeme (
   between (char '{') (char '}') typedName
     <|> List.Literal <$> P.ident
     <|> List.Literal . pack <$> some (punctuationChar <|> symbolChar)
   )

  where
    typedName :: P.Parser List.ParseTerm
    typedName = do
      ident <- P.ident
      _ <- char ':'
      ty <- choice [P.lstr "num"]
      case ty of
        "num" -> pure $ List.Number ident
        _     -> fail ("Unexpected type in line parse spec: " ++ show ty)

parsedLine :: [List.ParseTerm] -> P.Parser V.Value
parsedLine terms = V.ParsedLine . M.fromList <$> line
  where
    line = catMaybes <$> traverse termParser terms

    termParser :: List.ParseTerm -> P.Parser (Maybe (Text, V.Value))
    termParser (List.Literal str) = string str *> pure Nothing
    termParser (List.Number name) = do
      v <- P.integer
      pure $ Just (name, v)
