{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module HSolv.Pretty
  ( prettyExpr
  , prettyNum
  , prettyBool
  ) where

import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as Text
import HSolv.Expr

prettyExpr :: SomeExpr -> Text
prettyExpr (SomeExpr expr) = case expr of
  NumLit {} -> prettyNum expr
  Var {} -> prettyNum expr
  Add {} -> prettyNum expr
  Mul {} -> prettyNum expr
  Pow {} -> prettyNum expr
  Neg {} -> prettyNum expr
  Sin {} -> prettyNum expr
  Cos {} -> prettyNum expr
  Tan {} -> prettyNum expr
  Exp {} -> prettyNum expr
  Log {} -> prettyNum expr
  Sqrt {} -> prettyNum expr
  Abs {} -> prettyNum expr
  If {} -> prettyNum expr
  BoolLit {} -> prettyBool expr
  Eq {} -> prettyBool expr
  Lt {} -> prettyBool expr
  Le {} -> prettyBool expr
  Gt {} -> prettyBool expr
  Ge {} -> prettyBool expr
  And {} -> prettyBool expr
  Or {} -> prettyBool expr
  Not {} -> prettyBool expr

prettyNum :: NumExpr -> Text
prettyNum = prettyNumPrec 0

prettyBool :: BoolExpr -> Text
prettyBool = prettyBoolPrec 0

prettyNumPrec :: Int -> NumExpr -> Text
prettyNumPrec _ (NumLit r) = prettyRational r
prettyNumPrec _ (Var name) = name
prettyNumPrec p (Add a b) = parensIf (p > 1) (prettyNumPrec 1 a <> " + " <> prettyNumPrec 1 b)
prettyNumPrec p (Mul a b) = parensIf (p > 2) (prettyNumPrec 2 a <> " * " <> prettyNumPrec 2 b)
prettyNumPrec p (Pow a b) = parensIf (p > 3) (prettyNumPrec 3 a <> " ^ " <> prettyNumPrec 4 b)
prettyNumPrec p (Neg a) = parensIf (p > 4) ("-" <> prettyNumPrec 4 a)
prettyNumPrec _ (Sin a) = "sin(" <> prettyNumPrec 0 a <> ")"
prettyNumPrec _ (Cos a) = "cos(" <> prettyNumPrec 0 a <> ")"
prettyNumPrec _ (Tan a) = "tan(" <> prettyNumPrec 0 a <> ")"
prettyNumPrec _ (Exp a) = "exp(" <> prettyNumPrec 0 a <> ")"
prettyNumPrec _ (Log a) = "log(" <> prettyNumPrec 0 a <> ")"
prettyNumPrec _ (Sqrt a) = "sqrt(" <> prettyNumPrec 0 a <> ")"
prettyNumPrec _ (Abs a) = "abs(" <> prettyNumPrec 0 a <> ")"
prettyNumPrec _ (If c t f) =
  "if " <> prettyBoolPrec 0 c <> " then " <> prettyNumPrec 0 t <> " else " <> prettyNumPrec 0 f

prettyBoolPrec :: Int -> BoolExpr -> Text
prettyBoolPrec _ (BoolLit True) = "true"
prettyBoolPrec _ (BoolLit False) = "false"
prettyBoolPrec p (And a b) = parensIf (p > 1) (prettyBoolPrec 1 a <> " && " <> prettyBoolPrec 1 b)
prettyBoolPrec p (Or a b) = parensIf (p > 0) (prettyBoolPrec 0 a <> " || " <> prettyBoolPrec 0 b)
prettyBoolPrec p (Not a) = parensIf (p > 2) ("!" <> prettyBoolPrec 2 a)
prettyBoolPrec _ (Eq a b) = prettyNumPrec 0 a <> " == " <> prettyNumPrec 0 b
prettyBoolPrec _ (Lt a b) = prettyNumPrec 0 a <> " < " <> prettyNumPrec 0 b
prettyBoolPrec _ (Le a b) = prettyNumPrec 0 a <> " <= " <> prettyNumPrec 0 b
prettyBoolPrec _ (Gt a b) = prettyNumPrec 0 a <> " > " <> prettyNumPrec 0 b
prettyBoolPrec _ (Ge a b) = prettyNumPrec 0 a <> " >= " <> prettyNumPrec 0 b

prettyRational :: Rational -> Text
prettyRational r
  | d == 1 = Text.pack (show n)
  | otherwise = Text.pack (show n) <> "/" <> Text.pack (show d)
  where
    n = numerator r
    d = denominator r

parensIf :: Bool -> Text -> Text
parensIf True t = "(" <> t <> ")"
parensIf False t = t
