{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module HSolv.ExprUtil
  ( ExprTag(..)
  , exprTag
  , matchSome
  ) where

import HSolv.Expr

data ExprTag (t :: Type) where
  NumTag :: ExprTag 'TNum
  BoolTag :: ExprTag 'TBool

exprTag :: Expr t -> ExprTag t
exprTag expr = case expr of
  NumLit {} -> NumTag
  Var {} -> NumTag
  Add {} -> NumTag
  Mul {} -> NumTag
  Pow {} -> NumTag
  Neg {} -> NumTag
  Sin {} -> NumTag
  Cos {} -> NumTag
  Tan {} -> NumTag
  Exp {} -> NumTag
  Log {} -> NumTag
  Sqrt {} -> NumTag
  Abs {} -> NumTag
  If {} -> NumTag
  BoolLit {} -> BoolTag
  Eq {} -> BoolTag
  Lt {} -> BoolTag
  Le {} -> BoolTag
  Gt {} -> BoolTag
  Ge {} -> BoolTag
  And {} -> BoolTag
  Or {} -> BoolTag
  Not {} -> BoolTag

matchSome :: SomeExpr -> (NumExpr -> r) -> (BoolExpr -> r) -> r
matchSome (SomeExpr expr) onNum onBool =
  case exprTag expr of
    NumTag -> onNum expr
    BoolTag -> onBool expr
