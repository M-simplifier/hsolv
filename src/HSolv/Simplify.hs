{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module HSolv.Simplify
  ( simplifyNum
  , simplifyBool
  , simplifySome
  ) where

import Data.Ratio (denominator, numerator)
import HSolv.Expr

simplifySome :: SomeExpr -> SomeExpr
simplifySome (SomeExpr expr) = case expr of
  NumLit {} -> SomeExpr (simplifyNum expr)
  Var {} -> SomeExpr (simplifyNum expr)
  Add {} -> SomeExpr (simplifyNum expr)
  Mul {} -> SomeExpr (simplifyNum expr)
  Pow {} -> SomeExpr (simplifyNum expr)
  Neg {} -> SomeExpr (simplifyNum expr)
  Sin {} -> SomeExpr (simplifyNum expr)
  Cos {} -> SomeExpr (simplifyNum expr)
  Tan {} -> SomeExpr (simplifyNum expr)
  Exp {} -> SomeExpr (simplifyNum expr)
  Log {} -> SomeExpr (simplifyNum expr)
  Sqrt {} -> SomeExpr (simplifyNum expr)
  Abs {} -> SomeExpr (simplifyNum expr)
  If {} -> SomeExpr (simplifyNum expr)
  BoolLit {} -> SomeExpr (simplifyBool expr)
  Eq {} -> SomeExpr (simplifyBool expr)
  Lt {} -> SomeExpr (simplifyBool expr)
  Le {} -> SomeExpr (simplifyBool expr)
  Gt {} -> SomeExpr (simplifyBool expr)
  Ge {} -> SomeExpr (simplifyBool expr)
  And {} -> SomeExpr (simplifyBool expr)
  Or {} -> SomeExpr (simplifyBool expr)
  Not {} -> SomeExpr (simplifyBool expr)

simplifyNum :: NumExpr -> NumExpr
simplifyNum expr = case expr of
  NumLit r -> NumLit r
  Var name -> Var name
  Neg a -> case simplifyNum a of
    NumLit r -> NumLit (-r)
    Neg b -> b
    a' -> Neg a'
  Add a b ->
    let a' = simplifyNum a
        b' = simplifyNum b
    in case (a', b') of
      (NumLit 0, x) -> x
      (x, NumLit 0) -> x
      (NumLit x, NumLit y) -> NumLit (x + y)
      _ -> Add a' b'
  Mul a b ->
    let a' = simplifyNum a
        b' = simplifyNum b
    in case (a', b') of
      (NumLit 0, _) -> NumLit 0
      (_, NumLit 0) -> NumLit 0
      (NumLit 1, x) -> x
      (x, NumLit 1) -> x
      (NumLit x, NumLit y) -> NumLit (x * y)
      _ -> Mul a' b'
  Pow a b ->
    let a' = simplifyNum a
        b' = simplifyNum b
    in case (a', b') of
      (_, NumLit 0) -> NumLit 1
      (x, NumLit 1) -> x
      (NumLit x, NumLit y) ->
        case rationalToInt y of
          Just n -> NumLit (x ^^ n)
          Nothing -> Pow a' b'
      _ -> Pow a' b'
  Sin a ->
    let a' = simplifyNum a
    in case a' of
      NumLit 0 -> NumLit 0
      _ -> Sin a'
  Cos a ->
    let a' = simplifyNum a
    in case a' of
      NumLit 0 -> NumLit 1
      _ -> Cos a'
  Tan a ->
    let a' = simplifyNum a
    in case a' of
      NumLit 0 -> NumLit 0
      _ -> Tan a'
  Exp a ->
    let a' = simplifyNum a
    in case a' of
      NumLit 0 -> NumLit 1
      _ -> Exp a'
  Log a ->
    let a' = simplifyNum a
    in case a' of
      NumLit 1 -> NumLit 0
      _ -> Log a'
  Sqrt a ->
    let a' = simplifyNum a
    in case a' of
      NumLit 0 -> NumLit 0
      NumLit 1 -> NumLit 1
      _ -> Sqrt a'
  Abs a ->
    let a' = simplifyNum a
    in case a' of
      NumLit r -> NumLit (abs r)
      _ -> Abs a'
  If cond t f ->
    let cond' = simplifyBool cond
        t' = simplifyNum t
        f' = simplifyNum f
    in case cond' of
      BoolLit True -> t'
      BoolLit False -> f'
      _ -> If cond' t' f'

simplifyBool :: BoolExpr -> BoolExpr
simplifyBool expr = case expr of
  BoolLit b -> BoolLit b
  And a b ->
    let a' = simplifyBool a
        b' = simplifyBool b
    in case (a', b') of
      (BoolLit True, x) -> x
      (x, BoolLit True) -> x
      (BoolLit False, _) -> BoolLit False
      (_, BoolLit False) -> BoolLit False
      _ -> And a' b'
  Or a b ->
    let a' = simplifyBool a
        b' = simplifyBool b
    in case (a', b') of
      (BoolLit True, _) -> BoolLit True
      (_, BoolLit True) -> BoolLit True
      (BoolLit False, x) -> x
      (x, BoolLit False) -> x
      _ -> Or a' b'
  Not a ->
    let a' = simplifyBool a
    in case a' of
      BoolLit b -> BoolLit (not b)
      Not b -> b
      _ -> Not a'
  Eq a b ->
    let a' = simplifyNum a
        b' = simplifyNum b
    in case (a', b') of
      (NumLit x, NumLit y) -> BoolLit (x == y)
      _ -> Eq a' b'
  Lt a b -> simplifyCompare Lt (<) a b
  Le a b -> simplifyCompare Le (<=) a b
  Gt a b -> simplifyCompare Gt (>) a b
  Ge a b -> simplifyCompare Ge (>=) a b

simplifyCompare
  :: (NumExpr -> NumExpr -> BoolExpr)
  -> (Rational -> Rational -> Bool)
  -> NumExpr
  -> NumExpr
  -> BoolExpr
simplifyCompare ctor op a b =
  let a' = simplifyNum a
      b' = simplifyNum b
  in case (a', b') of
    (NumLit x, NumLit y) -> BoolLit (op x y)
    _ -> ctor a' b'

rationalToInt :: Rational -> Maybe Integer
rationalToInt r =
  if denominator r == 1
    then Just (numerator r)
    else Nothing
