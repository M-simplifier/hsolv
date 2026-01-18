# CLI/TUI の構成

対応コード:
- CLI: `app/Main.hs`
- TUI: `app/Tui.hs`
- 共通コマンド: `src/HSolv/Command.hs`

方針:
- コマンド処理は `Command` と `runCommand` に集約
- UI 層は結果の表示のみを担当
- TUI は Brick で実装し、入力/出力/補完/ドキュメントのパネルを持つ

TUI の操作:
- Enter: 実行
- Tab: 補完
- Up/Down: 履歴
- Esc/Ctrl-C: 終了

UI を分離したことで、CLI と TUI が同じロジックを共有しつつ違う表現を持てます。
