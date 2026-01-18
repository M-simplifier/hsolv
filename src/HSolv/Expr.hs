{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module HSolv.Expr
  ( Type(..)
  , Expr(..)
  , SomeExpr(..)
  , NumExpr
  , BoolExpr
  ) where

import Data.Text (Text)

data Type = TNum | TBool

data Expr (t :: Type) where
  NumLit :: !Rational -> Expr 'TNum
  BoolLit :: !Bool -> Expr 'TBool
  Var :: !Text -> Expr 'TNum
  Add :: !(Expr 'TNum) -> !(Expr 'TNum) -> Expr 'TNum
  Mul :: !(Expr 'TNum) -> !(Expr 'TNum) -> Expr 'TNum
  Pow :: !(Expr 'TNum) -> !(Expr 'TNum) -> Expr 'TNum
  Neg :: !(Expr 'TNum) -> Expr 'TNum
  Sin :: !(Expr 'TNum) -> Expr 'TNum
  Cos :: !(Expr 'TNum) -> Expr 'TNum
  Tan :: !(Expr 'TNum) -> Expr 'TNum
  Exp :: !(Expr 'TNum) -> Expr 'TNum
  Log :: !(Expr 'TNum) -> Expr 'TNum
  Sqrt :: !(Expr 'TNum) -> Expr 'TNum
  Abs :: !(Expr 'TNum) -> Expr 'TNum
  Eq :: !(Expr 'TNum) -> !(Expr 'TNum) -> Expr 'TBool
  Lt :: !(Expr 'TNum) -> !(Expr 'TNum) -> Expr 'TBool
  Le :: !(Expr 'TNum) -> !(Expr 'TNum) -> Expr 'TBool
  Gt :: !(Expr 'TNum) -> !(Expr 'TNum) -> Expr 'TBool
  Ge :: !(Expr 'TNum) -> !(Expr 'TNum) -> Expr 'TBool
  And :: !(Expr 'TBool) -> !(Expr 'TBool) -> Expr 'TBool
  Or :: !(Expr 'TBool) -> !(Expr 'TBool) -> Expr 'TBool
  Not :: !(Expr 'TBool) -> Expr 'TBool
  If :: !(Expr 'TBool) -> !(Expr 'TNum) -> !(Expr 'TNum) -> Expr 'TNum

data SomeExpr where
  SomeExpr :: Expr t -> SomeExpr

type NumExpr = Expr 'TNum
type BoolExpr = Expr 'TBool
