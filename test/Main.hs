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
        , testSimplifyIdentity
        , testDiffSquare
        , testEvalNumeric
        , testSolveQuadratic
        , testSolveLinear
        , testSolveNonPolynomial
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

testSimplifyIdentity :: TestResult
testSimplifyIdentity =
  let expr = Add (NumLit 0) (Var "x")
  in assertEq "simplify identity" "x" (prettyNum (simplifyNum expr))

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
