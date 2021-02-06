{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Graph.Parser
       ( list
       , listInput
       , parsedLine
       ) where

import           Data.Text
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe (fromMaybe, maybe, catMaybes)
import qualified Graph.Ast as Graph
import qualified Parser as P
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Value as V

listInput :: Text -> P.Parser V.Value -> P.Parser [V.Value]
listInput sep pValue = P.ws *> sepBy pValue (P.lstr sep) <* eof

list :: P.Parser Graph.Problem
list = do
  _ <- P.ws *> P.lstr "graph" *> P.lstr "at"
  file <- P.filePath
  preprocessing <- optional preprocessingSteps
  terms <- parseTerms
  fromGraph <- P.value
  _ <- P.lstr "to"
  toGraph <- P.value
  _ <- P.lstr "solution"
  code <- P.code
  eof
  pure $
    Graph.GraphProblem {
      Graph.at            = file
    , Graph.preprocessing = fromMaybe [] preprocessing
    , Graph.solution      = code
    , Graph.parseTerms    = terms
    , Graph.fromEdge      = fromGraph
    , Graph.toEdge        = toGraph
    }

preprocessingSteps :: P.Parser [Graph.PreprocessingStep]
preprocessingSteps = P.lstr "preprocess" *> manyTill preprocessingStep (lookAhead $ P.lstr "where")
  where
    preprocessingStep = Graph.Strip <$> (P.lstr "strip" *> P.lexeme P.textLiteral)

parseTerms :: P.Parser [Graph.ParseTerm]
parseTerms = P.phrase ["where", "each", "line", "parses", "as"] *> manyTill term (P.phrase ["builds", "graph", "from"])

nonIdentifiers :: S.Set Char
nonIdentifiers = S.fromList $ ['!'..'/'] ++ [':'..'@'] ++ ['['..'`'] ++ ['|'..'~']

term :: P.Parser Graph.ParseTerm
term = P.lexeme (
   between (char '{') (char '}') typedName
     <|> P.lstr "[|many_sep_by" *> (Graph.ManySepBy <$> P.lexeme P.textLiteral <*> manyTill term (P.lstr "|]"))
     <|> Graph.Literal <$> P.ident
     <|> Graph.Literal . pack <$> some nonIdentifierExceptCurly
   )

  where
    nonIdentifierExceptCurly :: P.Parser Char
    nonIdentifierExceptCurly =
      oneOf nonIdentifiers

    typedName :: P.Parser Graph.ParseTerm
    typedName = do
      ident <- P.ident
      _ <- char ':'
      ty <- choice $ P.lstr <$> ["num", "char", "text", "word"]
      case ty of
        "num" -> pure $ Graph.Number ident
        "char" -> pure $ Graph.Char ident
        "text" -> pure $ Graph.Text ident
        "word" -> pure $ Graph.Word ident
        _     -> fail ("Unexpected type in line parse spec: " ++ show ty)

parsedLine :: Text -> [Graph.ParseTerm] -> P.Parser V.Value
parsedLine sep terms = V.ParsedLine . M.fromList <$> line
  where
    line = catMaybes <$> traverse termParser terms

    termParser :: Graph.ParseTerm -> P.Parser (Maybe (Text, V.Value))
    termParser (Graph.Literal str) = string str *> pure Nothing
    termParser (Graph.Char name) = Just . (name,) . V.Ch <$> (P.ws *> (alphaNumChar <|> punctuationChar <|> symbolChar) <* P.ws)
    -- I'm so sorry, future me
    termParser (Graph.Text name) = Just . (name,) . V.Txt . T.concat <$> (P.ws *> (manyTill charText $ lookAhead ((P.lstr sep >> pure ()) <|> eof)))
    termParser (Graph.Number name) = Just . (name,) <$> P.integer

    charText :: P.Parser Text
    charText = do
      c <- asciiChar
      pure $ pack [c]
