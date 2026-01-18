# 微分のルール

対応コード: `src/HSolv/Diff.hs`

微分はルールベースで実装しています。代表的なルール:

- `d/dx (x) = 1`
- `d/dx (a + b) = a' + b'`
- `d/dx (a * b) = a' * b + a * b'`
- `d/dx (sin a) = cos a * a'`
- `d/dx (exp a) = exp a * a'`

二次関数などの典型例でテストすると、簡約と合わせて読みやすい形になります。
