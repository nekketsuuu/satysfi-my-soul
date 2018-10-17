# 基本的な数式

## math 型の値

`${ ... }` で囲われた部分は数式として扱われます。

```satysfi
@require: standalone
@require: math

let-block ctx +p it =
  let ib = read-inline ctx it in
  let ib = inline-fil ++ ib ++ inline-fil in
  line-break false false ctx ib
in
standalone '<
%% BEGIN
+p {
  二次方程式 ${ax^2 + bx + c = 0} (ただし，${a \neq 0})
}
%% END
>
```

## 数式ブロック

数式からなるブロック要素を作るには、多くの場合 `+math` を使います。末尾にセミコロンが必要です。

```satysfi
@require: standalone
@require: math

standalone '<
%% BEGIN
+math (${
  x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}
});
%% END
>
```
