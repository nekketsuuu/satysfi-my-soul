# 括弧

このページでは、SATySFi の数式で括弧を扱う方法について書きます。math.satyh を使っています。

## 式を丸括弧で囲う

`\paren{ ... }` を使うと、中身を丸括弧で囲うことができます。

```satysfi
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+math (${
  \paren{x + a} \paren{x + b} = x^2 + \paren{a + b} x + ab
});
%% END
>
```

### 括弧の大きさ

`\paren{ ... }` の丸括弧は中身に応じて大きさが変わります。

```satysfi
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+math (${
  \paren{1 + a} \paren{1 + a^2} \paren{1 + a^{2^2}}
});
%% END
>
```

### 丸括弧を直接書くとエラー

丸括弧 `( ... )` を直接 math 内に書くと構文エラーとなります。丸括弧記号は数式とプログラムの境界を示すのに使われているためです。

```{.satysfi eval="error"}
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+math (${
  % これはエラー
  (x + a)(x + b) = x^2 + (a + b)x + ab
});
%% END
>
```

バックスラッシュでエスケープすることで、大きさの変わらない単なる丸括弧を書くことはできます。

<div class="result-size-middle" markdown="1">

```satysfi
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+align [
  [${}; ${\( 1 + a \)   \( 1 + a^2 \)   \( 1 + a^{2^2} \)}];
  [${}; ${\paren{1 + a} \paren{1 + a^2} \paren{1 + a^{2^2}}}];
];
%% END
>
```

</div>

## 色々な形の括弧

丸括弧以外の括弧も用意されています。

| 名前 | コマンド |
|:--|:--|
| 波括弧 `{ ... }` | `\brace` |
| 角括弧 `[ ... ]` | `\sqbracket` |
| 山括弧 `〈 ... 〉` | `\angle-bracket` |

<div class="result-size-middle" markdown="1">

```satysfi
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+align [
  [${\text!{波括弧}}; ${\colon-rel \brace{1 + x}}];
  [${\text!{角括弧}}; ${\colon-rel \sqbracket{1 + x}}];
  [${\text!{山括弧}}; ${\colon-rel \angle-bracket{1 + x}}];
];
%% END
>
```

</div>

## 集合

集合を表すためにはコマンド `\set` が使えます。縦棒によるセパレータが真ん中に入る `\setsep` も用意されています。

```satysfi
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+math (
  let-math \bmod = math-char MathBin `mod` in
  ${\set{0, 2, 4, 6, 8, \ldots}
    = \setsep{n \in \mathbb{N}}{n \bmod 2 = 0}}
);
%% END
>
```

## 関数適用

関数を適用するときの丸括弧 `f(x)` を表現するためには `\app` が使えます。`f \paren{x}` と書くより構造がはっきりするので便利です。

```satysfi
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+math (${
  \app{f}{x} = \sum_{n = 0}^\infty \frac{\app{f^{\paren{n}}}{a}}{n\!} \paren{x - a}^n
});
%% END
>
```
