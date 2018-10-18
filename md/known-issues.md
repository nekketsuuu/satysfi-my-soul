<!-- このページはまだリリースしていません -->

# 既知のバグ

## text-in-math と文字サイズ

text-in-math や math-char で埋め込んだ math を上付き文字や下付き文字にすると、文字サイズが大きいままになってしまう。gfn さんも把握されているが、それなりに大きな改修が必要になりそうとのこと。

```satysfi
@require: standalone-nekketsuuu

standalone '<
%% BEGIN
+math-list [
  ${2^\text!{Foo}};
  ${2^\mathrm{Foo}};
  ${2^\mathrm-token!(`Foo`)};
];
%% END
>
```

