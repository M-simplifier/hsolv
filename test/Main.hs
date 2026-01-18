{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Exit (exitFailure, exitSuccess)
import HSolv.Diff
import HSolv.Eval
import HSolv.Expr
import HSolv.Parser
import HSolv.Pretty
import HSolv.Simplify
import HSolv.Solve

data TestResult = Pass | Fail String

main :: IO ()
main = do
  let results =
        [ testParsePretty
        , testParseBool
        , testSimplifyIdentity
        , testSimplifyCancel
        , testDiffSquare
        , testEvalNumeric
        , testSolveQuadratic
        , testSolveLinear
        , testSolveNonPolynomial
        , propertySimplifyIdempotent
        , propertySimplifyPreservesEval
        , propertyPrettyRoundtrip
        ]
      failures = [msg | Fail msg <- results]
  if null failures
    then exitSuccess
    else do
      putStrLn ("Failures:\n" <> intercalate "\n" failures)
      exitFailure

testParsePretty :: TestResult
testParsePretty =
  case parseNumText "1 + 2*3" of
    Left err -> Fail ("parse failed: " <> Text.unpack err)
    Right expr ->
      assertEq "pretty parse" "1 + 2 * 3" (prettyNum expr)

testParseBool :: TestResult
testParseBool =
  case parseBoolText "x < 3 && true" of
    Left err -> Fail ("parse bool failed: " <> Text.unpack err)
    Right expr ->
      assertEq "pretty bool" "x < 3 && true" (prettyBool expr)

testSimplifyIdentity :: TestResult
testSimplifyIdentity =
  let expr = Add (NumLit 0) (Var "x")
  in assertEq "simplify identity" "x" (prettyNum (simplifyNum expr))

testSimplifyCancel :: TestResult
testSimplifyCancel =
  let expr = Add (Var "x") (Neg (Var "x"))
  in assertEq "simplify cancel" "x + -x" (prettyNum (simplifyNum expr))

testDiffSquare :: TestResult
testDiffSquare =
  let expr = Mul (Var "x") (Var "x")
      deriv = simplifyNum (diff "x" expr)
  in assertEq "diff square" "x + x" (prettyNum deriv)

testEvalNumeric :: TestResult
testEvalNumeric =
  case parseNumText "x^2 + 2*x + 1" of
    Left err -> Fail ("parse failed: " <> Text.unpack err)
    Right expr ->
      case evalNum (Map.fromList [("x", 3)]) expr of
        Left err -> Fail ("eval failed: " <> Text.unpack err)
        Right value ->
          assertApprox "eval quadratic" 16 value

testSolveQuadratic :: TestResult
testSolveQuadratic =
  case parseNumText "x^2 - 5*x + 6" of
    Left err -> Fail ("parse failed: " <> Text.unpack err)
    Right expr ->
      case solveQuadratic "x" expr of
        Left err -> Fail ("solve failed: " <> Text.unpack err)
        Right roots ->
          assertEq "solve quadratic" ["3", "2"] (map prettyNum roots)

testSolveLinear :: TestResult
testSolveLinear =
  case parseNumText "2*x - 8" of
    Left err -> Fail ("parse failed: " <> Text.unpack err)
    Right expr ->
      case solveQuadratic "x" expr of
        Left err -> Fail ("solve failed: " <> Text.unpack err)
        Right roots ->
          assertEq "solve linear" ["4"] (map prettyNum roots)

testSolveNonPolynomial :: TestResult
testSolveNonPolynomial =
  case parseNumText "sin(x)" of
    Left err -> Fail ("parse failed: " <> Text.unpack err)
    Right expr ->
      case solveQuadratic "x" expr of
        Left _ -> Pass
        Right _ -> Fail "solve non-polynomial should fail"

propertySimplifyIdempotent :: TestResult
propertySimplifyIdempotent =
  property "simplify idempotent" 200 17 $ \expr ->
    let once = simplifyNum expr
        twice = simplifyNum once
    in prettyNum once == prettyNum twice

propertySimplifyPreservesEval :: TestResult
propertySimplifyPreservesEval =
  property "simplify preserves eval" 200 23 $ \expr ->
    let env = Map.fromList [("x", 2.0), ("y", -3.0)]
    in case (evalNum env expr, evalNum env (simplifyNum expr)) of
        (Right a, Right b) -> approxEq 1.0e-9 a b
        _ -> False

propertyPrettyRoundtrip :: TestResult
propertyPrettyRoundtrip =
  propertyWithExample "pretty roundtrip" 200 101 $ \expr ->
    let rendered = prettyNum expr
    in case parseNumText rendered of
        Left _ -> Left ("parse failed for: " <> Text.unpack rendered)
        Right parsed ->
          if prettyNum parsed == rendered
            then Right ()
            else Left ("roundtrip mismatch: " <> Text.unpack rendered <> " -> " <> Text.unpack (prettyNum parsed))

assertEq :: (Eq a, Show a) => String -> a -> a -> TestResult
assertEq label expected actual =
  if expected == actual
    then Pass
    else Fail (label <> ": expected " <> show expected <> " got " <> show actual)

assertApprox :: String -> Double -> Double -> TestResult
assertApprox label expected actual =
  let epsilon = 1.0e-9
  in if abs (expected - actual) <= epsilon
      then Pass
      else Fail (label <> ": expected " <> show expected <> " got " <> show actual)

property :: String -> Int -> Int -> (NumExpr -> Bool) -> TestResult
property label samples seed predicate =
  if all predicate (take samples (generateExprs seed))
    then Pass
    else Fail (label <> ": property failed")

propertyWithExample :: String -> Int -> Int -> (NumExpr -> Either String ()) -> TestResult
propertyWithExample label samples seed predicate =
  go 0 (generateExprs seed)
  where
    go count (expr:rest)
      | count >= samples = Pass
      | otherwise = case predicate expr of
          Right () -> go (count + 1) rest
          Left msg -> Fail (label <> ": " <> msg)
    go _ [] = Fail (label <> ": no samples generated")

approxEq :: Double -> Double -> Double -> Bool
approxEq eps a b = abs (a - b) <= eps

generateExprs :: Int -> [NumExpr]
generateExprs seed =
  let (expr, nextSeed) = genExpr 4 seed
  in expr : generateExprs nextSeed

genExpr :: Int -> Int -> (NumExpr, Int)
genExpr depth seed =
  let (choice, seed1) = nextInt seed
      pick = choice `mod` 6
  in if depth <= 0
      then genAtom seed1
      else case pick of
        0 -> genAtom seed1
        1 -> genBinary Add depth seed1
        2 -> genBinary Mul depth seed1
        3 -> genUnary Neg depth seed1
        4 -> genPow depth seed1
        _ -> genBinary Add depth seed1

genAtom :: Int -> (NumExpr, Int)
genAtom seed =
  let (choice, seed1) = nextInt seed
  in case choice `mod` 3 of
      0 -> (NumLit (fromInteger (toInteger (choice `mod` 7))), seed1)
      1 -> (Var "x", seed1)
      _ -> (Var "y", seed1)

genUnary :: (NumExpr -> NumExpr) -> Int -> Int -> (NumExpr, Int)
genUnary ctor depth seed =
  let (expr, seed1) = genExpr (depth - 1) seed
  in (ctor expr, seed1)

genBinary :: (NumExpr -> NumExpr -> NumExpr) -> Int -> Int -> (NumExpr, Int)
genBinary ctor depth seed =
  let (a, seed1) = genExpr (depth - 1) seed
      (b, seed2) = genExpr (depth - 1) seed1
  in (ctor a b, seed2)

genPow :: Int -> Int -> (NumExpr, Int)
genPow depth seed =
  let (base, seed1) = genExpr (depth - 1) seed
      (expoChoice, seed2) = nextInt seed1
      expo = NumLit (fromInteger (toInteger (expoChoice `mod` 3)))
  in (Pow base expo, seed2)

nextInt :: Int -> (Int, Int)
nextInt seed =
  let next = (1103515245 * seed + 12345) `mod` 2147483647
  in (next, next)
