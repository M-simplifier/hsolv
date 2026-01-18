{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module HSolv.Eval
  ( Value(..)
  , Env
  , evalNum
  , evalBool
  , evalSome
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import HSolv.Expr
import HSolv.ExprUtil

data Value = VNum !Double | VBool !Bool

type Env = Map Text Double

evalSome :: Env -> SomeExpr -> Either Text Value
evalSome env expr =
  matchSome expr (fmap VNum . evalNum env) (fmap VBool . evalBool env)

evalNum :: Env -> NumExpr -> Either Text Double
evalNum env expr = case expr of
  NumLit r -> Right (fromRational r)
  Var name -> case Map.lookup name env of
    Just v -> Right v
    Nothing -> Left ("unknown variable: " <> name)
  Add a b -> (+) <$> evalNum env a <*> evalNum env b
  Mul a b -> (*) <$> evalNum env a <*> evalNum env b
  Pow a b -> (**) <$> evalNum env a <*> evalNum env b
  Neg a -> negate <$> evalNum env a
  Sin a -> sin <$> evalNum env a
  Cos a -> cos <$> evalNum env a
  Tan a -> tan <$> evalNum env a
  Exp a -> exp <$> evalNum env a
  Log a -> log <$> evalNum env a
  Sqrt a -> sqrt <$> evalNum env a
  Abs a -> abs <$> evalNum env a
  If cond t f -> do
    cond' <- evalBool env cond
    if cond' then evalNum env t else evalNum env f

evalBool :: Env -> BoolExpr -> Either Text Bool
evalBool env expr = case expr of
  BoolLit b -> Right b
  And a b -> (&&) <$> evalBool env a <*> evalBool env b
  Or a b -> (||) <$> evalBool env a <*> evalBool env b
  Not a -> not <$> evalBool env a
  Eq a b -> (==) <$> evalNum env a <*> evalNum env b
  Lt a b -> (<) <$> evalNum env a <*> evalNum env b
  Le a b -> (<=) <$> evalNum env a <*> evalNum env b
  Gt a b -> (>) <$> evalNum env a <*> evalNum env b
  Ge a b -> (>=) <$> evalNum env a <*> evalNum env b
