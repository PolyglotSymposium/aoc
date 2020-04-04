{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Test.Hspec

import qualified Aoc
import qualified Ast
import           Builtins (listContext)
import qualified Type
import qualified TypeCheck
import qualified Value as V

number = Ast.Inte 42
numTy  = Type.Number
boolTy = Type.Boolean

sub a b  = Ast.Subtract a b
div' a b = Ast.Divide a b

a &&& b = Ast.And a b

gt = Ast.Gt

ident = Ast.Identifier

lam = Ast.FloatingLambda . Ast.Body

(|>) = Ast.Pipe

shouldBeUnificationFailureOf (Left (TypeCheck.UnificationFailure _ _ a b _)) (c, d) =
  (a, b) `shouldBe` (c, d)

shouldBeUnificationFailureOf v _ =
  error $ "Expected unification failure, got " ++ show v

main :: IO ()
main = hspec $ do
  describe "Aoc.solve" $ do
    it "solves 2015 D1 P1" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2015d1p1.aoc"
      result `shouldBe` 280

    it "solves 2015 D1 P2 (Current)" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2015d1p2.aoc"
      result `shouldBe` 1797

    it "solves 2015 D18 P1" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2015d18p1.aoc"
      result `shouldBe` 821

    it "solves 2015 D18 P2" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2015d18p2.aoc"
      result `shouldBe` 886

    it "solves 2015 D23 P1" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2015d23p1.aoc"
      result `shouldBe` 184

    it "solves 2015 D23 P2" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2015d23p2.aoc"
      result `shouldBe` 231

    it "solves 2016 D18 P1" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2016d18p1.aoc"
      result `shouldBe` 1939

    it "solves 2016 D12 P1" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2016d12p1.aoc"
      result `shouldBe` 318020

    it "solves 2016 D12 P2" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2016d12p2.aoc"
      result `shouldBe` 9227674

    it "solves 2017 D8 P1" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2017d8p1.aoc"
      result `shouldBe` 6828

    it "solves 2017 D8 P2" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2017d8p2.aoc"
      result `shouldBe` 7234

    it "solves 2018 D1 P1" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2018d1p1.aoc"
      result `shouldBe` 595

    it "solves 2018 D1 P2" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2018d1p2.aoc"
      result `shouldBe` 80598

    it "solves 2018 D18 P1" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2018d18p1.aoc"
      result `shouldBe` 560091

    it "solves 2019 D1 P1" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2019d1p1.aoc"
      result `shouldBe` 3308377

    it "solves 2019 D1 P2" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2019d1p2.aoc"
      result `shouldBe` 4959709

    it "solves 2019 D24 P1" $ do
      Just (_, _, V.I result, _) <- Aoc.solve "./examples/y2019d24p1.aoc"
      result `shouldBe` 18844281

  describe "TypeCheck.ensureOneFreeOrIdentInEachStep" $ do
    it "finds the identifier in a simple &&" $
      TypeCheck.ensureOneFreeOrIdentInEachStep listContext (lam (number &&& ident "x")) `shouldBe` Right ()

    it "finds the identifier some piped together steps" $
      TypeCheck.ensureOneFreeOrIdentInEachStep listContext ((lam (number &&& ident "x")) |> (lam (ident "x" `div'` number))) `shouldBe` Right ()

  describe "TypeCheck.unify" $ do
    it "numbers unify with numbers" $
      TypeCheck.unify listContext number numTy Nothing `shouldBe` Right Nothing

    it "numbers don't unify with bools" $
      TypeCheck.unify listContext number boolTy Nothing `shouldBeUnificationFailureOf` (boolTy, numTy)

    it "simple free variables can be unified" $
      TypeCheck.unify listContext (ident "a") numTy Nothing `shouldBe` Right (Just numTy)

    it "mismatches when already unified cause errors" $
      TypeCheck.unify listContext (ident "a") numTy (Just boolTy) `shouldBeUnificationFailureOf` (boolTy, numTy)

    it "unifications can successfully match operands" $
      TypeCheck.unify listContext (ident "a" `sub` ident "a") numTy Nothing `shouldBe` Right (Just numTy)

    it "unification works on builtin identifiers" $
      TypeCheck.unify listContext (ident "true") boolTy Nothing `shouldBe` Right (Just boolTy)

    it "when unification fails on builtins the errors make sense" $
      TypeCheck.unify listContext (ident "true") numTy Nothing `shouldBeUnificationFailureOf` (numTy, boolTy)

    it "unification flows down the tree and to the right" $
      TypeCheck.unify listContext (ident "a" &&& (ident "a" `gt` number)) boolTy Nothing `shouldBeUnificationFailureOf` (boolTy, numTy)

    it "silly fat test" $
      TypeCheck.unify listContext ((ident "a" `sub` ident "a") `gt` ident "a") boolTy Nothing `shouldBe` Right (Just numTy)

    it "can unify a left-side variable" $
      TypeCheck.unify listContext ((ident "x" `div'` number) `sub` number) numTy Nothing `shouldBe` Right (Just numTy)
