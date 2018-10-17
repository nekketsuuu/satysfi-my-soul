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
