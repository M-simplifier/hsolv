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

data Value = VNum !Double | VBool !Bool

type Env = Map Text Double

evalSome :: Env -> SomeExpr -> Either Text Value
evalSome env (SomeExpr expr) = case expr of
  NumLit {} -> VNum <$> evalNum env expr
  Var {} -> VNum <$> evalNum env expr
  Add {} -> VNum <$> evalNum env expr
  Mul {} -> VNum <$> evalNum env expr
  Pow {} -> VNum <$> evalNum env expr
  Neg {} -> VNum <$> evalNum env expr
  Sin {} -> VNum <$> evalNum env expr
  Cos {} -> VNum <$> evalNum env expr
  Tan {} -> VNum <$> evalNum env expr
  Exp {} -> VNum <$> evalNum env expr
  Log {} -> VNum <$> evalNum env expr
  Sqrt {} -> VNum <$> evalNum env expr
  Abs {} -> VNum <$> evalNum env expr
  If {} -> VNum <$> evalNum env expr
  BoolLit {} -> VBool <$> evalBool env expr
  Eq {} -> VBool <$> evalBool env expr
  Lt {} -> VBool <$> evalBool env expr
  Le {} -> VBool <$> evalBool env expr
  Gt {} -> VBool <$> evalBool env expr
  Ge {} -> VBool <$> evalBool env expr
  And {} -> VBool <$> evalBool env expr
  Or {} -> VBool <$> evalBool env expr
  Not {} -> VBool <$> evalBool env expr

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
