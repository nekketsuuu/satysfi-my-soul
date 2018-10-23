# 典型的なエラー

このページでは SATySFi を使っているときに出るエラーをいくつかピックアップし、その解決法を解説します。

## 必要なレコード要素不足

```{.satysfi eval="error"}
@require: stdjabook

document (|
  title = {渡す要素が足りない例};
  author = {\@nekketsuuu};
|) '<
>
```

「stdjabook.satyh の `document` に渡すレコードには `show-toc`, `show-title` という要素も必須だが、それらが足りない」ために起こっているエラーです。次のように書くと上手くいきます。

```{.satysfi eval="type-check-only"}
@require: stdjabook

document (|
  title = {渡す要素が足りている例};
  author = {\@nekketsuuu};
  show-toc = false;
  show-title = true;
|) '<
>
```

## 末尾のセミコロン

### インラインテキストでの例

```{.satysfi eval="error"}
@require: stdja
@require: standalone

standalone '<
%% BEGIN
+p {
  \SATySFi
}
%% END
>
```

引数を取らないインライン・コマンドの末尾にはセミコロンが必要です。

```{.satysfi eval="type-check-only"}
@require: stdja
@require: standalone

standalone '<
%% BEGIN
+p {
  \SATySFi;
}
%% END
>
```

### 数式ブロックの例

```{.satysfi eval="error"}
@require: math
@require: standalone

standalone '<
%% BEGIN
+math (${
  ...
})
%% END
>
```

`+math` に渡す式引数の末尾にはセミコロンが必要です。

```{.satysfi eval="type-check-only"}
@require: math
@require: standalone

standalone '<
%% BEGIN
+math (${
  ...
});
%% END
>
```
