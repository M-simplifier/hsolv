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
    simplifyAdd (simplifyNum a) (simplifyNum b)
  Mul a b ->
    simplifyMul (simplifyNum a) (simplifyNum b)
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

simplifyAdd :: NumExpr -> NumExpr -> NumExpr
simplifyAdd left right =
  let terms = collectAdd left <> collectAdd right
      (constSum, termGroups) = foldl addTerm (0, []) terms
      builtTerms = map (uncurry buildCoeff) termGroups
      allTerms = builtTerms <> (if constSum == 0 then [] else [NumLit constSum])
  in buildAdd allTerms

collectAdd :: NumExpr -> [NumExpr]
collectAdd expr = case expr of
  Add a b -> collectAdd a <> collectAdd b
  _ -> [expr]

addTerm :: (Rational, [(Rational, NumExpr)]) -> NumExpr -> (Rational, [(Rational, NumExpr)])
addTerm (k, groups) expr = case expr of
  NumLit r -> (k + r, groups)
  _ ->
    let (coef, term) = splitCoeff expr
    in if coef == 0
        then (k, groups)
        else (k, insertOrAdd coef term groups)

insertOrAdd :: Rational -> NumExpr -> [(Rational, NumExpr)] -> [(Rational, NumExpr)]
insertOrAdd coef term [] = [(coef, term)]
insertOrAdd coef term ((k, t):rest)
  | eqNum term t =
      let k' = k + coef
      in if k' == 0 then rest else (k', t) : rest
  | otherwise = (k, t) : insertOrAdd coef term rest

buildAdd :: [NumExpr] -> NumExpr
buildAdd [] = NumLit 0
buildAdd (t:ts) = foldl Add t ts

simplifyMul :: NumExpr -> NumExpr -> NumExpr
simplifyMul left right =
  let factors = collectMul left <> collectMul right
      (prod, rest) = foldl mulFactor (1, []) factors
  in if prod == 0
      then NumLit 0
      else buildMul prod rest

collectMul :: NumExpr -> [NumExpr]
collectMul expr = case expr of
  Mul a b -> collectMul a <> collectMul b
  _ -> [expr]

mulFactor :: (Rational, [NumExpr]) -> NumExpr -> (Rational, [NumExpr])
mulFactor (k, rest) expr = case expr of
  NumLit r -> (k * r, rest)
  Neg t ->
    let (k', rest') = mulFactor (k, rest) t
    in (-k', rest')
  _ -> (k, rest <> [expr])

buildMul :: Rational -> [NumExpr] -> NumExpr
buildMul k terms =
  let normalized = filter (not . isOne) terms
      base = if k == 1 then normalized else NumLit k : normalized
  in case base of
      [] -> NumLit 1
      (t:ts) -> foldl Mul t ts

isOne :: NumExpr -> Bool
isOne expr = case expr of
  NumLit r -> r == 1
  _ -> False

splitCoeff :: NumExpr -> (Rational, NumExpr)
splitCoeff expr = case expr of
  NumLit r -> (r, NumLit 1)
  Mul (NumLit r) t -> (r, t)
  Neg t ->
    let (k, core) = splitCoeff t
    in (-k, core)
  _ -> (1, expr)

buildCoeff :: Rational -> NumExpr -> NumExpr
buildCoeff k term
  | k == 0 = NumLit 0
  | k == 1 = term
  | k == -1 = Neg term
  | eqNum term (NumLit 1) = NumLit k
  | otherwise = Mul (NumLit k) term

eqNum :: NumExpr -> NumExpr -> Bool
eqNum a b = case (a, b) of
  (NumLit x, NumLit y) -> x == y
  (Var x, Var y) -> x == y
  (Add a1 b1, Add a2 b2) ->
    (eqNum a1 a2 && eqNum b1 b2) || (eqNum a1 b2 && eqNum b1 a2)
  (Mul a1 b1, Mul a2 b2) ->
    (eqNum a1 a2 && eqNum b1 b2) || (eqNum a1 b2 && eqNum b1 a2)
  (Pow a1 b1, Pow a2 b2) -> eqNum a1 a2 && eqNum b1 b2
  (Neg x, Neg y) -> eqNum x y
  (Sin x, Sin y) -> eqNum x y
  (Cos x, Cos y) -> eqNum x y
  (Tan x, Tan y) -> eqNum x y
  (Exp x, Exp y) -> eqNum x y
  (Log x, Log y) -> eqNum x y
  (Sqrt x, Sqrt y) -> eqNum x y
  (Abs x, Abs y) -> eqNum x y
  (If c1 t1 f1, If c2 t2 f2) -> eqBool c1 c2 && eqNum t1 t2 && eqNum f1 f2
  _ -> False

eqBool :: BoolExpr -> BoolExpr -> Bool
eqBool a b = case (a, b) of
  (BoolLit x, BoolLit y) -> x == y
  (And a1 b1, And a2 b2) -> eqBool a1 a2 && eqBool b1 b2
  (Or a1 b1, Or a2 b2) -> eqBool a1 a2 && eqBool b1 b2
  (Not x, Not y) -> eqBool x y
  (Eq a1 b1, Eq a2 b2) -> eqNum a1 a2 && eqNum b1 b2
  (Lt a1 b1, Lt a2 b2) -> eqNum a1 a2 && eqNum b1 b2
  (Le a1 b1, Le a2 b2) -> eqNum a1 a2 && eqNum b1 b2
  (Gt a1 b1, Gt a2 b2) -> eqNum a1 a2 && eqNum b1 b2
  (Ge a1 b1, Ge a2 b2) -> eqNum a1 a2 && eqNum b1 b2
  _ -> False
