# 設計の全体像

hsolv は「式の型安全性」を中心に組み立てられています。核となる AST を定義し、その周辺にパーサ、簡約、評価、微分、方程式ソルバを配置します。

主要モジュール:
- `src/HSolv/Expr.hs`: 型レベルで数式と真偽式を分離する AST
- `src/HSolv/ExprUtil.hs`: `SomeExpr` の型分岐を行う共通ユーティリティ
- `src/HSolv/Parser.hs`: テキストから AST への変換
- `src/HSolv/Simplify.hs`: 正規化・簡約のルール
- `src/HSolv/Eval.hs`: 数値評価
- `src/HSolv/Diff.hs`: 記号微分
- `src/HSolv/Solve.hs`: 二次方程式の実数解
- `src/HSolv/Command.hs`: CLI/TUI 共通のコマンド層
- `app/Main.hs`, `app/Tui.hs`: UI 層

設計の方針:
- コアは純粋関数で構成し、副作用は UI 層に限定
- 型で「数式 / 真偽式」を分離し、誤用をコンパイル時に防ぐ
- 簡約は「正規化 → ルール適用 → 出力整形」を意識して設計

データフロー:

1. 入力テキストをパースして `Expr` を構築
2. `simplify` によって正規化された AST を得る
3. `pretty` でユーザーに表示
4. `eval` や `diff`、`solve` は `Expr` を入力に受け取る
