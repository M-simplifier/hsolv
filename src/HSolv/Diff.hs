{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module HSolv.Diff
  ( diff
  ) where

import Data.Text (Text)
import HSolv.Expr

diff :: Text -> NumExpr -> NumExpr
diff var expr = case expr of
  NumLit _ -> NumLit 0
  Var name -> if name == var then NumLit 1 else NumLit 0
  Add a b -> Add (diff var a) (diff var b)
  Mul a b -> Add (Mul (diff var a) b) (Mul a (diff var b))
  Pow base expo -> case expo of
    NumLit r -> Mul (Mul (NumLit r) (Pow base (NumLit (r - 1)))) (diff var base)
    _ ->
      Mul
        (Pow base expo)
        (Add (Mul (diff var expo) (Log base))
             (Mul expo (Mul (diff var base) (Pow base (NumLit (-1))))))
  Neg a -> Neg (diff var a)
  Sin a -> Mul (Cos a) (diff var a)
  Cos a -> Neg (Mul (Sin a) (diff var a))
  Tan a -> Mul (Pow (Cos a) (NumLit (-2))) (diff var a)
  Exp a -> Mul (Exp a) (diff var a)
  Log a -> Mul (Pow a (NumLit (-1))) (diff var a)
  Sqrt a -> Mul (Pow (Sqrt a) (NumLit (-1))) (Mul (NumLit (1/2)) (diff var a))
  Abs a -> Mul (Pow (Abs a) (NumLit (-1))) (Mul a (diff var a))
  If cond t f -> If cond (diff var t) (diff var f)
