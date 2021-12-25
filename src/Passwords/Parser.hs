{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Passwords.Parser
  ( problem
  ) where

import           Control.Monad                  ( void )
import           Data.Bifunctor
import           Data.Foldable                  ( asum )
import qualified Data.Graph                    as G
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , isJust
                                                )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import qualified Parser                        as P
import qualified Passwords.Ast                 as Passwords
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Value                         as V

problem :: P.Parser Passwords.Problem
problem = do
  void $ P.ws *> P.lstr "passwords" *> P.lstr "at"
  file <- P.filePath
  void $ P.phrase ["are", "valid", "if", "they"]
  rules         <- some validityRule
  _             <- P.lstr "solution"
  code          <- P.code
  eof
  pure $ Passwords.PasswordsProblem
    { Passwords.at              = file
    , Passwords.validityRules   = rules
    , Passwords.solution        = code
    }

validityRule :: P.Parser Passwords.ValidityRule
validityRule = containRule <|> notContainRule

containRule :: P.Parser Passwords.ValidityRule
containRule =
  P.lstr "contain" *> (Passwords.Contain <$> amountOrAny <*> substringMatch)

dumbPluralOrSingular word s = try (P.lstr $ word <> s) <|> P.lstr word

substringMatch :: P.Parser Passwords.SubstringMatch
substringMatch = asum
  [ P.lstr "of" *> (Passwords.AnyCharOf <$> P.simpleQuoted)
  , dumbPluralOrSingular "substring" "s" *> substringRule
  ]

substringRule :: P.Parser Passwords.SubstringMatch
substringRule = asum
  [ Passwords.AnySubstringOf <$> sepBy1 P.simpleQuoted (P.lstr ",")
  , P.lstr "that" *> matchesRule
  ]

matchesRule :: P.Parser Passwords.SubstringMatch
matchesRule = do
  greed <- optional $ P.lstr "greedily"
  void $ dumbPluralOrSingular "match" "es"
  patt <- P.simpleQuoted
  pure $ maybe (Passwords.SubstringMatching patt) (const $ Passwords.SubstringGreedilyMatching patt) greed

amountOrAny :: P.Parser Passwords.Amount
amountOrAny = fromMaybe Passwords.Any <$> optional amount

amount :: P.Parser Passwords.Amount
amount =
  P.phrase ["at", "least"] *> (Passwords.AtLeast <$> P.lexeme P.rawInteger)

notContainRule :: P.Parser Passwords.ValidityRule
notContainRule =
  P.phrase ["do", "not", "contain"]
    *> (Passwords.DoNotContain <$> amountOrAny <*> substringMatch)
