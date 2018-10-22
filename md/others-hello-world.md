# Hello, World!

SATySFi による Hello World です。

<div class="result-size-full" markdown="1">

```satysfi
@require: stdjabook

document (|
  title = {Hello, World!};
  author = {\@nekketsuuu};
  show-toc = false;
  show-title = true;
|) '<
  +p {
    Hello, World!
  }
>
```

</div>

こちらは、もう少し凝った Hello World です。

```satysfi
@require: stdja
@require: standalone

let-inline \show-string s =
  embed-string s
in
standalone '<
 +p {
   \show-string(`Hello, World!`);
 }
>
```
