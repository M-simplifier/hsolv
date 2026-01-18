{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Ratio (numerator)
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
    , runQC "pretty bool roundtrip" propPrettyBoolRoundtrip
    , runQC "simplify bool preserves eval" propSimplifyBoolPreservesEval
    , runQC "diff matches numeric (poly2)" propDiffMatchesNumeric
    , runQC "solve quadratic roots" propSolveQuadraticRoots
    , runQC "solve linear root" propSolveLinearRoot
    , runQC "solve rejects non-polynomial" propSolveRejectsNonPoly
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
      (Right a, Right b) -> property (eqFloat 1.0e-9 a b)
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

propPrettyBoolRoundtrip :: BoolExprGen -> Property
propPrettyBoolRoundtrip (BoolExprGen expr) =
  let rendered = prettyBool expr
  in case parseBoolText rendered of
      Left _ -> counterexample ("parse failed for: " <> Text.unpack rendered) False
      Right parsed ->
        counterexample
          ("roundtrip mismatch: " <> Text.unpack rendered <> " -> " <> Text.unpack (prettyBool parsed))
          (prettyBool parsed == rendered)

propSimplifyBoolPreservesEval :: BoolExprGen -> Property
propSimplifyBoolPreservesEval (BoolExprGen expr) =
  let env = Map.fromList [("x", 2.0), ("y", -3.0)]
  in case (evalBool env expr, evalBool env (simplifyBool expr)) of
      (Right a, Right b) -> property (a == b)
      _ -> property False

propDiffMatchesNumeric :: Poly2Gen -> Property
propDiffMatchesNumeric (Poly2Gen (a, b, c)) =
  forAll (choose (-5.0, 5.0)) $ \x ->
    let expr = polyExprFromCoeffs a b c
        deriv = diff "x" expr
        env = Map.fromList [("x", x)]
        dfx = evalNum env deriv
        approx = (evalNum (Map.fromList [("x", x + h)]) expr, evalNum (Map.fromList [("x", x - h)]) expr)
    in case (dfx, approx) of
        (Right dval, (Right fxh, Right fxm)) ->
          let numeric = (fxh - fxm) / (2 * h)
          in property (eqFloat 1.0e-6 dval numeric)
        _ -> property False
  where
    h = 1.0e-5

propSolveQuadraticRoots :: QuadCoeffsGen -> Property
propSolveQuadraticRoots (QuadCoeffsGen (a, b, c)) =
  let expr = polyExprFromCoeffs a b c
      disc = b * b - 4 * a * c
  in disc >= 0 ==>
      case solveQuadratic "x" expr of
        Left err -> counterexample ("solve failed: " <> Text.unpack err) False
        Right roots ->
          counterexample ("root count: " <> show (length roots)) (length roots == 2)
          .&&. property (all (rootSatisfies expr) roots)

propSolveLinearRoot :: LinearCoeffsGen -> Property
propSolveLinearRoot (LinearCoeffsGen (b, c)) =
  let expr = polyExprFromCoeffs 0 b c
  in case solveQuadratic "x" expr of
      Left err -> counterexample ("solve failed: " <> Text.unpack err) False
      Right roots ->
        counterexample ("root count: " <> show (length roots)) (length roots == 1)
        .&&. property (all (rootSatisfies expr) roots)

propSolveRejectsNonPoly :: NonPolyGen -> Property
propSolveRejectsNonPoly (NonPolyGen expr) =
  case solveQuadratic "x" expr of
    Left _ -> property True
    Right _ -> counterexample "expected failure for non-polynomial" False

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

eqFloat :: Double -> Double -> Double -> Bool
eqFloat eps a b
  | isNaN a && isNaN b = True
  | isInfinite a && isInfinite b = True
  | otherwise = abs (a - b) <= eps

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

newtype BoolExprGen = BoolExprGen BoolExpr

instance Show BoolExprGen where
  show (BoolExprGen expr) = Text.unpack (prettyBool expr)

instance Arbitrary BoolExprGen where
  arbitrary = BoolExprGen <$> sized genBoolExpr
  shrink (BoolExprGen expr) = BoolExprGen <$> shrinkBoolExpr expr

newtype QuadCoeffsGen = QuadCoeffsGen (Rational, Rational, Rational)

instance Show QuadCoeffsGen where
  show (QuadCoeffsGen (a, b, c)) = show (a, b, c)

instance Arbitrary QuadCoeffsGen where
  arbitrary = do
    a <- coeffNonZero
    b <- coeffAny
    c <- coeffAny
    pure (QuadCoeffsGen (a, b, c))
  shrink (QuadCoeffsGen (a, b, c)) =
    [ QuadCoeffsGen (a', b', c')
    | a' <- shrinkRationalNonZero a
    , b' <- shrinkRational b
    , c' <- shrinkRational c
    ]

newtype LinearCoeffsGen = LinearCoeffsGen (Rational, Rational)

instance Show LinearCoeffsGen where
  show (LinearCoeffsGen (b, c)) = show (b, c)

instance Arbitrary LinearCoeffsGen where
  arbitrary = do
    b <- coeffNonZero
    c <- coeffAny
    pure (LinearCoeffsGen (b, c))
  shrink (LinearCoeffsGen (b, c)) =
    [ LinearCoeffsGen (b', c')
    | b' <- shrinkRationalNonZero b
    , c' <- shrinkRational c
    ]

newtype Poly2Gen = Poly2Gen (Rational, Rational, Rational)

instance Show Poly2Gen where
  show (Poly2Gen (a, b, c)) = show (a, b, c)

instance Arbitrary Poly2Gen where
  arbitrary = do
    a <- coeffAny
    b <- coeffAny
    c <- coeffAny
    pure (Poly2Gen (a, b, c))
  shrink (Poly2Gen (a, b, c)) =
    [ Poly2Gen (a', b', c')
    | a' <- shrinkRational a
    , b' <- shrinkRational b
    , c' <- shrinkRational c
    ]

newtype NonPolyGen = NonPolyGen NumExpr

instance Show NonPolyGen where
  show (NonPolyGen expr) = Text.unpack (prettyNum expr)

instance Arbitrary NonPolyGen where
  arbitrary = oneof
    [ pure (NonPolyGen (Sin (Var "x")))
    , pure (NonPolyGen (Add (Sin (Var "x")) (NumLit 1)))
    , pure (NonPolyGen (Log (Add (Var "x") (NumLit 2))))
    ]

genExpr :: Int -> Gen NumExpr
genExpr size
  | size <= 1 = genAtom
  | otherwise = oneof
      [ genAtom
      , Add <$> genSub <*> genSub
      , Mul <$> genSub <*> genSub
      , Neg <$> genSub
      , Pow <$> genSub <*> genExpo
      , Sin <$> genSub
      , Cos <$> genSub
      , Tan <$> genSub
      , Exp <$> genSub
      , Log <$> genSub
      , Sqrt <$> genSub
      , Abs <$> genSub
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
  Sin a -> [a]
  Cos a -> [a]
  Tan a -> [a]
  Exp a -> [a]
  Log a -> [a]
  Sqrt a -> [a]
  Abs a -> [a]
  _ -> []

genBoolExpr :: Int -> Gen BoolExpr
genBoolExpr size
  | size <= 1 = genBoolAtom
  | otherwise = oneof
      [ genBoolAtom
      , And <$> genSub <*> genSub
      , Or <$> genSub <*> genSub
      , Not <$> genSub
      ]
  where
    genSub = genBoolExpr (size `div` 2)

genBoolAtom :: Gen BoolExpr
genBoolAtom = oneof
  [ pure (BoolLit True)
  , pure (BoolLit False)
  , Eq <$> genExpr 2 <*> genExpr 2
  , Lt <$> genExpr 2 <*> genExpr 2
  , Le <$> genExpr 2 <*> genExpr 2
  , Gt <$> genExpr 2 <*> genExpr 2
  , Ge <$> genExpr 2 <*> genExpr 2
  ]

shrinkBoolExpr :: BoolExpr -> [BoolExpr]
shrinkBoolExpr expr = case expr of
  And a b -> [a, b]
  Or a b -> [a, b]
  Not a -> [a]
  Eq a b -> [Eq a b]
  Lt a b -> [Lt a b]
  Le a b -> [Le a b]
  Gt a b -> [Gt a b]
  Ge a b -> [Ge a b]
  _ -> []

coeffAny :: Gen Rational
coeffAny = fromInteger <$> chooseInteger (-5, 5)

coeffNonZero :: Gen Rational
coeffNonZero = suchThat coeffAny (/= 0)

shrinkRational :: Rational -> [Rational]
shrinkRational r = [fromInteger n | n <- shrink (numerator r)]

shrinkRationalNonZero :: Rational -> [Rational]
shrinkRationalNonZero r = filter (/= 0) (shrinkRational r)

polyExprFromCoeffs :: Rational -> Rational -> Rational -> NumExpr
polyExprFromCoeffs a b c =
  Add
    (Mul (NumLit a) (Pow (Var "x") (NumLit 2)))
    (Add (Mul (NumLit b) (Var "x")) (NumLit c))

rootSatisfies :: NumExpr -> NumExpr -> Bool
rootSatisfies expr root =
  case evalNum Map.empty root of
    Left _ -> False
    Right rootVal ->
      case evalNum (Map.fromList [("x", rootVal)]) expr of
        Left _ -> False
        Right fx -> eqFloat 1.0e-6 fx 0
