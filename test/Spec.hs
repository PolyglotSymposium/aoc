{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import Test.QuickCheck

import qualified ListType
import qualified Type
import qualified ListAst

number = ListAst.Inte 42
numTy = Type.Number
boolTy = Type.Boolean

sub a b = ListAst.Subtract a b

ident = ListAst.Identifier

main :: IO ()
main = hspec $ do
  describe "ListType.unify" $ do
    it "numbers unify with numbers" $ do
      ListType.unify number numTy Nothing `shouldBe` Right Nothing

    it "simple free variables can be unified" $ do
      ListType.unify (ident "a") numTy Nothing `shouldBe` Right (Just numTy)

    it "mismatches when already unified cause errors" $ do
      ListType.unify (ident "a") numTy (Just boolTy) `shouldBe` Left (ListType.UnificationFailure (Just boolTy) numTy boolTy (ident "a"))

    it "unifications can successfully match operands" $ do
      ListType.unify (ident "a" `sub` ident "a") numTy Nothing `shouldBe` Right (Just numTy)

    it "unification works on builtin identifiers" $ do
      ListType.unify (ident "true") boolTy Nothing `shouldBe` Right Nothing
