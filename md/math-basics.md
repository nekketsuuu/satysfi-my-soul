# 基本的な数式

## math 型の値

`${ ... }` で囲われた部分は数式として扱われます。

```satysfi
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+p {
  二次方程式${ax^2 + bx + c = 0}（ただし，${a \neq 0}）
}
%% END
>
```

## 数式ブロック

数式からなるブロック要素を作るには、多くの場合 `+math` を使います。末尾にセミコロンが必要です。

```satysfi
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+math (${
  x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}
});
%% END
>
```

### 数式ブロック内でのプログラミング

`+math` に渡す引数の中にはプログラムを書くことができます。その場限りでよく使う数式を定義するのに便利です。

```satysfi
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+math (
  let limx = ${\lim_{x \to 0}} in
  ${
    #limx \frac{x - \sin x}{x^3}
    = #limx \frac{1 - \cos x}{3x^2}
    = #limx \frac{\sin x}{6x}
    = \frac{1}{6}
  }
);
%% END
>
```

<div class="box-note">

**中級者向け**: `+math` は標準の math.satyh で提供されているコマンドであり、組み込みコマンドではありません。このため、やろうと思えば自前で `+math` 相当のコマンドを作ることができます。

</div>

## 関連

* [数式](index.html#-2)
