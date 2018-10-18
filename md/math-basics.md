# 基本的な数式

このページでは、math.satyh を使って数式を扱う方法について書きます。

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

数式からなるブロック要素を作るには、`+math` を使います。末尾にセミコロンが必要です。

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

## 数式を並べる

複数の数式を縦に並べたブロックを作るには `+align` を使います。引数には数式のリストのリストを渡します。それぞれの数式を分割して渡すと、分割位置で縦に揃えて表示されます。

<div class="result-size-middle">

```satysfi
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+align [
  [${\forall a, b, c.\ a \circ \paren{b \circ c}}; ${= \paren{a \circ b} \circ c}];
  [${\exists e. \forall a.\ a \circ e}; ${= a}];
  [${\forall a. \exists x.\ a \circ x}; ${= e}];
];
%% END
>
```

</div>

<div class="box-note">

**注意**: The SATySFibook に載っている Future Work によると、`+align : [(math list) list] block-cmd` というインターフェースはぎこちないので将来的には LaTeX の `\align` 環境のようなインターフェースを用意することが検討されています。

</div>

## 関連

* [数式](index.html#-2)
