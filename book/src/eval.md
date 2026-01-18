# 評価器の設計

対応コード: `src/HSolv/Eval.hs`

`eval` は `Expr` を `Double` に評価します。記号計算は `Rational` ですが、評価は実数近似に落とします。

設計のポイント:
- 変数環境は `Map Text Double`
- 未定義の変数はエラー
- `if` は条件を真偽として評価

実装例（概略）:

```haskell
evalNum env (Add a b) = (+) <$> evalNum env a <*> evalNum env b
evalNum env (Var name) = maybe (Left err) Right (Map.lookup name env)
```

評価は副作用なしの純粋関数にすることで、テストや再利用が容易になります。
