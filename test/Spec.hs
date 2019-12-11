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
div' a b = ListAst.Divide a b

a &&& b = ListAst.And a b

gt = ListAst.Gt

ident = ListAst.Identifier

main :: IO ()
main = hspec $ do
  describe "ListType.unify" $ do
    it "numbers unify with numbers" $ do
      ListType.unify number numTy Nothing `shouldBe` Right Nothing

    it "numbers don't unify with bools" $ do
      ListType.unify number boolTy Nothing `shouldBe` Left (ListType.UnificationFailure Nothing boolTy numTy number)

    it "simple free variables can be unified" $ do
      ListType.unify (ident "a") numTy Nothing `shouldBe` Right (Just numTy)

    it "mismatches when already unified cause errors" $ do
      ListType.unify (ident "a") numTy (Just boolTy) `shouldBe` Left (ListType.UnificationFailure (Just boolTy) boolTy numTy (ident "a"))

    it "unifications can successfully match operands" $ do
      ListType.unify (ident "a" `sub` ident "a") numTy Nothing `shouldBe` Right (Just numTy)

    it "unification works on builtin identifiers" $ do
      ListType.unify (ident "true") boolTy Nothing `shouldBe` Right Nothing

    it "when unification fails on builtins the errors make sense" $ do
      ListType.unify (ident "true") numTy Nothing `shouldBe` Left (ListType.UnificationFailure Nothing numTy boolTy (ident "true"))

    it "unification flows down the tree and to the right" $ do
      ListType.unify (ident "a" &&& (ident "a" `gt` number)) boolTy Nothing `shouldBe` Left (ListType.UnificationFailure (Just boolTy) boolTy numTy (ident "a"))

    it "silly fat test" $ do
      ListType.unify ((ident "a" `sub` ident "a") `gt` ident "a") boolTy Nothing `shouldBe` Right (Just numTy)

    it "can unify a left-side variable" $ do
      ListType.unify ((ident "x" `div'` number) `sub` number) numTy Nothing `shouldBe` Right (Just numTy)

--    
