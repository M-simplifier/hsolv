# 式と型の設計

hsolv の核は GADT と DataKinds を使った AST です。数式と真偽式を型レベルで分離し、誤った演算がコンパイル時に落ちるようにします。

対応コード: `src/HSolv/Expr.hs`

要点:
- `Type = TNum | TBool` により型を明示
- `Expr 'TNum` と `Expr 'TBool` を分離
- `SomeExpr` により UI 層で型を隠蔽

例（抜粋）:

```haskell
data Expr (t :: Type) where
  NumLit :: Rational -> Expr 'TNum
  BoolLit :: Bool -> Expr 'TBool
  Add :: Expr 'TNum -> Expr 'TNum -> Expr 'TNum
  Eq  :: Expr 'TNum -> Expr 'TNum -> Expr 'TBool
```

ここで `Add` や `Eq` の型が制約を与えます。例えば `Add (BoolLit True) (NumLit 1)` はコンパイルできません。

`SomeExpr` は UI 側の柔軟性のために導入しています。パース段階では「数式か真偽式か」を決めきれないため、型を隠蔽した器が必要になります。
