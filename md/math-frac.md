# 分数

このページでは、数式として分数を出力する方法について書きます。

## 分数

通常の分数を表すには `\frac` コマンドを使います。

```satysfi
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+math (${
  x = \frac{a}{b}
});
%% END
>
```
