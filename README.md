# asz80ml Z80 Assembler

## asz80ml 仕様

**asz80ml** は Z80 マイクロプロセッサ用のアセンブラです。

- 入力ファイル未指定時は標準入力を使用
- 出力ファイル未指定時は `a.bin`
- 出力に `-` を指定すると標準出力
- 複数ファイル指定時は、指定順に処理される

## マクロとラベルの基本仕様

- ニーモニックとレジスタ名は **大文字小文字を区別しない**
- ラベル名・マクロ名は **大文字小文字を区別する**
- 未定義マクロ・未使用マクロも構文チェックのみ行われる場合がある

## アセンブラディレクティブ

### include / incbin

```asm
include 'file'
incbin 'file'
```

- ファイルをそのままテキスト／バイナリとして取り込む
- include パスを順に探索
- ファイル名の展開は行わない（`~` などは無効）

### データ定義

```asm
db / defb   ; バイト定義
dw / defw   ; ワード定義（リトルエンディアン）
dm / defm   ; 文字列定義
ds / defs   ; 領域確保
```

例：

```asm
db "HELLO", 0
dw 1234
ds 16, 0
```

### ラベル定義と equ

```asm
label: equ expression
```

- ラベルに式の値を割り当てる
- 式は即時評価が必要

## 条件アセンブル

```asm
if expression
  ...
else
  ...
endif
```

- expression が 0 以外なら if 側を採用
- 複数 else ブロックを持てる
- 条件部では **即時評価可能な式のみ** 使用可能

## マクロ定義

```asm
name: macro arg1, arg2, ...
  ...
endm
```

- 命令が来る位置でマクロを使用できる
- マクロ展開は **純粋なテキスト置換**
- マクロ内でラベルを生成可能

例：

```asm
makelabel name
name_label:
endm
```

## org / seek

```asm
org address
seek offset
```

- `org` は論理アドレスを変更（出力サイズは変えない）
- `seek` は出力ファイル位置を変更（パッチ用途）

## 式の仕様

### 使用可能な演算子（優先順位順）

- 条件演算子: `a ? b : c`
- ビット演算: `| ^ &`
- 比較演算: `== != < <= > >=`
- シフト: `<< >>`
- 四則演算: `+ - * / %`
- 単項演算: `~ + -`
- 括弧: `( )`

### 特殊要素

```asm
?label   ; ラベルが存在すれば 1、なければ 0
$        ; 現在のアドレス
```

- `?label` は forward reference をチェックしない
- C の `#ifdef` 相当の用途に使える

## 数値リテラル

- 10進 / 8進 / 16進 / 2進
- 任意基数指定（@）
- 文字リテラル `'A'`
- エスケープ文字対応

## ラベルの制約

- 使用可能文字: 英数字・`_`・`.`
- 数字始まりは禁止
- 大文字小文字は区別される
- `.` で始まるラベルは **ローカルラベル**

ローカルラベルは：

- include ファイル単位
- マクロ展開単位

でスコープが分離される。

## 即時評価が必要な箇所

以下では **前方参照は禁止**：

- `org`
- `seek`
- `equ`
- `ds` の第1引数
- `if` の条件式

それ以外の式では前方参照が許可される。

## todo

- [x] 数式
- [x] 変数指定 label: equ 10
- [x] label: macro a,b endm
- [x] if elif else endif
- [x] z80 nimonic (minimum)
- [x] org, label address
- [x] include
- [x] defb defw db dw defm dm
- [x] incbin
- [x] z80 nimonic (all)
- [x] output binary file
- [x] single quote string
- [x] '\\"' '\\' '\\'' '\\0' '\\100' '\\n', '\\r', '\\a', '\\t'　10, 13, 7, 9
- [x] char literal
