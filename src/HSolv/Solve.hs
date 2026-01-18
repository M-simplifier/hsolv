{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module HSolv.Solve
  ( solveQuadratic
  ) where

import Data.Ratio ((%), denominator, numerator)
import Data.Text (Text)
import HSolv.Expr
import HSolv.Simplify (simplifyNum)

data Poly2 = Poly2 !Rational !Rational !Rational

solveQuadratic :: Text -> NumExpr -> Either Text [NumExpr]
solveQuadratic var expr = do
  Poly2 a b c <- case toPoly var expr of
    Just poly -> Right poly
    Nothing ->
      Left ("cannot solve: expression is not a polynomial in " <> var)
  if a == 0
    then solveLinear b c
    else case quadraticRoots a b c of
      Left err -> Left err
      Right roots -> Right (map simplifyNum roots)

solveLinear :: Rational -> Rational -> Either Text [NumExpr]
solveLinear b c
  | b == 0 =
      Left "cannot solve: expression has no variable term"
  | otherwise =
      Right [simplifyNum (Mul (NumLit (-c)) (Pow (NumLit b) (NumLit (-1))))]

quadraticRoots :: Rational -> Rational -> Rational -> Either Text [NumExpr]
quadraticRoots a b c =
  let aExpr = NumLit a
      bExpr = NumLit b
      cExpr = NumLit c
      discR = b * b - 4 * a * c
  in if discR < 0
      then Left "cannot solve: discriminant is negative"
      else case sqrtRationalMaybe discR of
        Just s ->
          let denom = NumLit (2 * a)
              rootPlus = Mul (Add (Neg bExpr) (NumLit s)) (Pow denom (NumLit (-1)))
              rootMinus = Mul (Add (Neg bExpr) (NumLit (-s))) (Pow denom (NumLit (-1)))
          in Right [rootPlus, rootMinus]
        Nothing ->
          let twoA = Mul (NumLit 2) aExpr
              disc = Add (Pow bExpr (NumLit 2)) (Mul (NumLit (-4)) (Mul aExpr cExpr))
              sqrtDisc = Sqrt disc
              negB = Neg bExpr
              rootPlus = Mul (Add negB sqrtDisc) (Pow twoA (NumLit (-1)))
              rootMinus = Mul (Add negB (Neg sqrtDisc)) (Pow twoA (NumLit (-1)))
          in Right [rootPlus, rootMinus]

toPoly :: Text -> NumExpr -> Maybe Poly2
toPoly var expr = case expr of
  NumLit r -> Just (Poly2 0 0 r)
  Var name -> if name == var then Just (Poly2 0 1 0) else Nothing
  Add a b -> addPoly <$> toPoly var a <*> toPoly var b
  Mul a b -> do
    pa <- toPoly var a
    pb <- toPoly var b
    mulPoly pa pb
  Neg a -> scalePoly (-1) <$> toPoly var a
  Pow base expo -> case expo of
    NumLit 0 -> Just (Poly2 0 0 1)
    NumLit 1 -> toPoly var base
    NumLit 2 -> do
      p <- toPoly var base
      mulPoly p p
    _ -> Nothing
  Abs {} -> Nothing
  Sin {} -> Nothing
  Cos {} -> Nothing
  Tan {} -> Nothing
  Exp {} -> Nothing
  Log {} -> Nothing
  Sqrt {} -> Nothing
  If {} -> Nothing

addPoly :: Poly2 -> Poly2 -> Poly2
addPoly (Poly2 a1 b1 c1) (Poly2 a2 b2 c2) =
  Poly2 (a1 + a2) (b1 + b2) (c1 + c2)

scalePoly :: Rational -> Poly2 -> Poly2
scalePoly k (Poly2 a b c) = Poly2 (k * a) (k * b) (k * c)

mulPoly :: Poly2 -> Poly2 -> Maybe Poly2
mulPoly (Poly2 a1 b1 c1) (Poly2 a2 b2 c2) =
  let x4 = a1 * a2
      x3 = a1 * b2 + b1 * a2
      x2 = a1 * c2 + b1 * b2 + c1 * a2
      x1 = b1 * c2 + c1 * b2
      x0 = c1 * c2
  in if x4 /= 0 || x3 /= 0
      then Nothing
      else Just (Poly2 x2 x1 x0)

sqrtRationalMaybe :: Rational -> Maybe Rational
sqrtRationalMaybe r
  | r < 0 = Nothing
  | otherwise = do
      let n = numerator r
          d = denominator r
      n' <- integerSqrtExact n
      d' <- integerSqrtExact d
      pure (n' % d')

integerSqrtExact :: Integer -> Maybe Integer
integerSqrtExact n
  | n < 0 = Nothing
  | otherwise =
      let r = floor (sqrt (fromInteger n :: Double))
      in if r * r == n then Just r else Nothing
