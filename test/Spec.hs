{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Test.Hspec

import qualified Aoc
import qualified Ast
import           Builtins (listContext)
import           Control.StopWatch
import           System.Clock (toNanoSecs)
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

testPartWithBenchGen unsafeGetValue year day part expected = do
  it ("solves " ++ show year ++ " D" ++ show day ++ " P" ++ show part) $ do
    let path = "./examples/y" ++ show year ++ "d" ++ show day ++ "p" ++ show part ++ ".aoc"

    (domain:_) <- words <$> readFile path

    (Just (_, _, result, _), spec) <- stopWatch $ Aoc.solve path
    unsafeGetValue result `shouldBe` expected
    putStrLn $ ("EXAMPLE_OUTPUT " ++) $ concat
      [
        "{"
      , "\"year\":", show year
      , ",\"day\":", show day
      , ",\"part\":", show part
      , ",\"ns\":", show $ toNanoSecs spec
      , ",\"source\":", show $ path
      , ",\"domain\":", show $ domain
      , "}"
      ]

  pure ()

testPartWithBench =
  testPartWithBenchGen $ \(V.I v) -> v

testPartWithBenchText =
  testPartWithBenchGen $ \(V.Txt v) -> v

main :: IO ()
main = hspec $ do
  describe "Aoc.solve" $ do
    testPartWithBench 2015 1 1 280

    testPartWithBench 2015 1 2 1797

    testPartWithBench 2015 2 1 1598415

    testPartWithBench 2015 2 2 3812909

    testPartWithBench 2015 3 1 2592

    testPartWithBench 2015 5 1 258

    testPartWithBench 2015 5 2 53

    testPartWithBench 2015 18 1 821

    testPartWithBench 2015 18 2 886

    testPartWithBench 2015 23 1 184

    testPartWithBench 2015 23 2 231

    testPartWithBench 2016 1 1 279

    testPartWithBench 2016 1 2 163

    testPartWithBench 2016 3 1 993

    testPartWithBench 2016 12 1 318020

    testPartWithBench 2016 12 2 9227674

    testPartWithBench 2016 18 1 1939

    testPartWithBench 2017 8 1 6828

    testPartWithBench 2017 8 2 7234

    testPartWithBench 2018 1 1 595

    testPartWithBench 2018 1 2 80598

    testPartWithBenchText 2018 7 1 "OVXCKZBDEHINPFSTJLUYRWGAMQ"

    testPartWithBench 2018 18 1 560091

    testPartWithBench 2019 1 1 3308377

    testPartWithBench 2019 1 2 4959709

    testPartWithBench 2019 6 1 154386

    testPartWithBench 2019 6 2 346

    testPartWithBench 2019 24 1 18844281

    testPartWithBench 2020 1 1 1007104

    testPartWithBench 2020 1 2 18847752

    testPartWithBench 2020 2 1 469

    testPartWithBench 2020 2 2 267

    testPartWithBench 2020 4 1 233

    testPartWithBench 2020 7 1 185

    testPartWithBench 2020 8 1 1766

    testPartWithBench 2020 11 1 2108

    testPartWithBench 2020 12 1 1601

    testPartWithBench 2020 17 1 273

    testPartWithBench 2020 17 2 1504

    testPartWithBench 2021 1 1 1696

    testPartWithBench 2021 1 2 1737

    testPartWithBench 2021 2 1 1813801

    testPartWithBench 2021 2 2 1960569556

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
