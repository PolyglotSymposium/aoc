{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Graph.Parser
       ( list
       , inputGraph
       ) where

import qualified Data.Graph as G
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Bifunctor
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.List (foldl')
import           Data.Void (Void)
import           Data.Maybe (fromMaybe, catMaybes, isJust)
import qualified Graph.Ast as Graph
import qualified Parser as P
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Value as V

list :: P.Parser Graph.Problem
list = do
  _ <- P.ws *> P.lstr "graph" *> P.lstr "at"
  file <- P.filePath
  preprocessing <- optional preprocessingSteps
  _ <- P.lstr "where"
  edgeDesignator <- P.lexeme $ P.phrase ["edges", "are", "denoted", "by"] *> P.textLiteral
  leftNode <- nodeTerms "left"
  rightNode <- nodeTerms "right"
  _ <- P.phrase ["builds", "graph", "from"]
  fromNode <- P.ident
  _ <- P.lstr "to"
  toNode <- P.ident
  bidirectional <- optional $ P.phrase ["and", "vice", "versa"]
  _ <- P.lstr "solution"
  code <- P.code
  eof
  pure $
    Graph.GraphProblem {
      Graph.at              = file
    , Graph.preprocessing   = fromMaybe [] preprocessing
    , Graph.edgeDesignator  = edgeDesignator
    , Graph.leftTerms       = leftNode
    , Graph.rightTerms      = rightNode
    , Graph.fromNode        = fromNode
    , Graph.toNode          = toNode
    , Graph.isBidirectional = isJust bidirectional
    , Graph.solution        = code
    }

nodeTerms :: Text -> P.Parser Graph.NodeTerms
nodeTerms side = P.lstr side *> (try multiple <|> single)

  where
    multiple = do
      _ <- P.phrase ["nodes", "are", "separated", "by"]
      sep <- P.lexeme P.textLiteral
      _ <- P.phrase ["and", "parse", "as"]
      terms <- parseTerms
      pure $ Graph.ManyNodesSepBy sep terms

    single = Graph.SingleNode <$> (P.phrase ["node", "parses", "as"] *> parseTerms)

preprocessingSteps :: P.Parser [Graph.PreprocessingStep]
preprocessingSteps = P.lstr "preprocess" *> manyTill preprocessingStep (lookAhead $ P.lstr "where")
  where
    preprocessingStep = Graph.Strip <$> (P.lstr "strip" *> P.lexeme P.textLiteral)

parseTerms :: P.Parser [Graph.ParseTerm]
parseTerms = manyTill term $ lookAhead (P.lstr "left" <|> P.lstr "builds" <|> P.lstr "right")

nonIdentifiers :: S.Set Char
nonIdentifiers = S.fromList $ ['!'..'/'] ++ [':'..'@'] ++ ['['..'`'] ++ ['|'..'~']

term :: P.Parser Graph.ParseTerm
term = P.lexeme (
   between (char '{') (char '}') typedName
     <|> Graph.Literal <$> P.ident
     <|> Graph.Literal . T.pack <$> some nonIdentifierExceptCurly
   )

  where
    nonIdentifierExceptCurly :: P.Parser Char
    nonIdentifierExceptCurly =
      oneOf nonIdentifiers

    typedName :: P.Parser Graph.ParseTerm
    typedName = do
      ident <- P.ident
      _ <- char ':'
      ty <- choice $ P.lstr <$> ["num", "char", "text"]
      case ty of
        "num" -> pure $ Graph.Number ident
        "char" -> pure $ Graph.Char ident
        "text" -> pure $ Graph.Text ident
        _     -> fail ("Unexpected type in line parse spec: " ++ show ty)

inputGraph :: Graph.Problem -> Text -> Either (ParseError Char Void) V.Value
inputGraph  Graph.GraphProblem{..} raw =
  V.Graph . G.listGraphToMapGraph <$> edges

  where
    preprocessed = foldl' preprocess raw preprocessing

    preprocess t (Graph.Strip needle) = T.replace needle "" t

    edges = fmap G.EquallyWeighted $ sequence $ do
      (lineNumber, (lhss, rhss)) <- zip [(1::Int)..] sides
      lhs <- filter (not . T.null) lhss
      rhs <- filter (not . T.null) rhss
      directionality $ do
        leftMap <- runParser (parsedTerms (Graph.terms leftTerms)) ("Left-hand side:" <> show lineNumber) lhs
        rightMap <- runParser (parsedTerms (Graph.terms rightTerms)) ("Right-hand side:" <> show lineNumber) rhs
        toEdge lineNumber $ M.union leftMap rightMap

    directionality (Right (a, b)) | isBidirectional = [Right (a, b), Right (b, a)]
    directionality (Right (a, b)) = [Right (a, b)]
    directionality err = [err]

    toEdge lineNumber mp =
      case (M.lookup fromNode mp, M.lookup toNode mp) of
        (Just (V.Txt from), Just (V.Txt to)) -> pure (G.Keyed from, G.Keyed to)
        nonText -> fail $ show lineNumber <> ": for " <> show (fromNode, toNode) <> " found " <> show nonText <> show mp

    sides :: [([Text], [Text])]
    sides =
      bimap (nodeSpan leftTerms) (nodeSpan rightTerms . T.drop (T.length edgeDesignator)) . T.breakOn edgeDesignator <$> T.splitOn "\n" preprocessed

    nodeSpan (Graph.SingleNode _) = pure . T.strip
    nodeSpan (Graph.ManyNodesSepBy sep _) = fmap T.strip . T.splitOn sep

parsedTerms :: [Graph.ParseTerm] -> P.Parser (M.Map Text V.Value)
parsedTerms terms = M.fromList <$> line
  where
    line = catMaybes <$> traverse termParser terms

    termParser :: Graph.ParseTerm -> P.Parser (Maybe (Text, V.Value))
    termParser (Graph.Literal str) = string str *> pure Nothing
    termParser (Graph.Char name) = Just . (name,) . V.Ch <$> (P.ws *> (alphaNumChar <|> punctuationChar <|> symbolChar) <* P.ws)
    -- I'm so sorry, future me
    termParser (Graph.Text name) = Just . (name,) . V.Txt . T.concat <$> (P.ws *> manyTill charText eof)
    termParser (Graph.Number name) = Just . (name,) <$> P.integer

    charText :: P.Parser Text
    charText = do
      c <- asciiChar
      pure $ T.pack [c]
