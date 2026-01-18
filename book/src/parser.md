# パーサの作り方

対応コード: `src/HSolv/Parser.hs`

方針:
- 優先順位を `chainl1` / `chainr1` で明示
- 数式と真偽式を別の入口として定義
- 失敗時は入力先頭をヒントとして返す

要点:
- 数式の優先順位: `+` < `*` < `^` < 単項 `-`
- 真偽式の優先順位: `||` < `&&` < `!`
- 比較は数式側に依存

例:

```haskell
parseNumExpr = chainl1 parseNumTerm addOp
parseNumTerm = chainl1 parseNumPow mulOp
parseNumPow  = chainr1 parseNumUnary powOp
```

設計上の注意:
- `if ... then ... else ...` は数式のみ許可
- `!=` は `Not (Eq ...)` として扱う
- 変数は英字で始まる識別子のみ許可
