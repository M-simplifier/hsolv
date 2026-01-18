{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck
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
  qcResults <- sequence
    [ runQC "simplify idempotent" propSimplifyIdempotent
    , runQC "simplify preserves eval" propSimplifyPreservesEval
    , runQC "pretty roundtrip" propPrettyRoundtrip
    ]
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
        ]
      failures = [msg | Fail msg <- (results <> qcResults)]
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

propSimplifyIdempotent :: NumExprGen -> Bool
propSimplifyIdempotent (NumExprGen expr) =
  let simplified = simplifyNum expr
      twice = simplifyNum simplified
  in prettyNum simplified == prettyNum twice

propSimplifyPreservesEval :: NumExprGen -> Property
propSimplifyPreservesEval (NumExprGen expr) =
  let env = Map.fromList [("x", 2.0), ("y", -3.0)]
  in case (evalNum env expr, evalNum env (simplifyNum expr)) of
      (Right a, Right b) -> property (approxEq 1.0e-9 a b)
      _ -> property False

propPrettyRoundtrip :: NumExprGen -> Property
propPrettyRoundtrip (NumExprGen expr) =
  let rendered = prettyNum expr
  in case parseNumText rendered of
      Left _ -> counterexample ("parse failed for: " <> Text.unpack rendered) False
      Right parsed ->
        counterexample
          ("roundtrip mismatch: " <> Text.unpack rendered <> " -> " <> Text.unpack (prettyNum parsed))
          (prettyNum parsed == rendered)

assertEq :: (Eq a, Show a) => String -> a -> a -> TestResult
assertEq name expected actual =
  if expected == actual
    then Pass
    else Fail (name <> ": expected " <> show expected <> " got " <> show actual)

assertApprox :: String -> Double -> Double -> TestResult
assertApprox name expected actual =
  let epsilon = 1.0e-9
  in if abs (expected - actual) <= epsilon
      then Pass
      else Fail (name <> ": expected " <> show expected <> " got " <> show actual)

approxEq :: Double -> Double -> Double -> Bool
approxEq eps a b = abs (a - b) <= eps

runQC :: Testable prop => String -> prop -> IO TestResult
runQC name prop = do
  result <- quickCheckWithResult qcArgs prop
  case result of
    Success {} -> pure Pass
    GaveUp {output = out} -> pure (Fail (name <> ": gave up\n" <> out))
    Failure {output = out} -> pure (Fail (name <> ": failed\n" <> out))
    NoExpectedFailure {output = out} -> pure (Fail (name <> ": unexpected success\n" <> out))

qcArgs :: Args
qcArgs = stdArgs {maxSuccess = 200, maxSize = 8}

newtype NumExprGen = NumExprGen NumExpr

instance Show NumExprGen where
  show (NumExprGen expr) = Text.unpack (prettyNum expr)

instance Arbitrary NumExprGen where
  arbitrary = NumExprGen <$> sized genExpr
  shrink (NumExprGen expr) = NumExprGen <$> shrinkExpr expr

genExpr :: Int -> Gen NumExpr
genExpr size
  | size <= 1 = genAtom
  | otherwise = oneof
      [ genAtom
      , Add <$> genSub <*> genSub
      , Mul <$> genSub <*> genSub
      , Neg <$> genSub
      , Pow <$> genSub <*> genExpo
      ]
  where
    genSub = genExpr (size `div` 2)

genAtom :: Gen NumExpr
genAtom = oneof
  [ NumLit . fromInteger <$> chooseInteger (0, 7)
  , pure (Var "x")
  , pure (Var "y")
  ]

genExpo :: Gen NumExpr
genExpo = NumLit . fromInteger <$> chooseInteger (0, 3)

shrinkExpr :: NumExpr -> [NumExpr]
shrinkExpr expr = case expr of
  Add a b -> [a, b]
  Mul a b -> [a, b]
  Neg a -> [a]
  Pow a _ -> [a]
  _ -> []
