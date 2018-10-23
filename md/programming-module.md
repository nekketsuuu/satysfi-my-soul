# モジュール

このページでは、SATySFi で使える「モジュール」という機能について解説します。

<div class="box-note" markdown="1">

**注意**: **SATySFi のモジュールに関する公式ドキュメントは執筆時点ではまだ無いので、このページに書かれていることは変更される可能性があります。** SATySFi version 0.0.3 時点では OCaml のモジュールと構文が似ているので、OCaml のマニュアル ["Chapter 2  The module system"](https://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html) や M.Hiroi さんの[「お気楽 OCaml プログラミング入門 モジュール」](http://www.geocities.jp/m_hiroi/func/ocaml09.html)もある程度参考になります。

</div>

## モジュールの作成

SATySFi におけるモジュールは、名前空間を切り分けつつ実装を抽象化したいときに便利な言語機能です。`module 名前 = struct 定義列 end` という記法で作成することができます。通常の変数名と異なり、モジュール名の 1 文字目は大文字でなければいけません。

```{.satysfi eval="type-check-only"}
module ExampleModule = struct
  let foo = 42
  let bar n = n + 42
end
```

## モジュール内の変数へのアクセス

モジュール `M` 内にある `foo` という変数は `M.foo` とピリオドでつなぐことで指定できます。

```satysfi
@require: stdja
@require: standalone

module ExampleModule = struct
  let foo = 42
  let bar n = n + 42
end

let-inline \show-int n =
  embed-string (arabic n)
in
standalone '<
%% BEGIN
+p {
  foo = \show-int(ExampleModule.foo);
}
+p {
  bar 42 = \show-int(ExampleModule.bar 42);
}
%% END
>
```

<div class="box-note" markdown="1">

**中級者向け**: SATySFi version 0.0.3 ではまだドキュメントされていませんが、module open に関して以下の構文が用意されています ([Issue #79](https://github.com/gfngfn/SATySFi/issues/79))。

* `open M` という構文でそこから下において全域的にモジュールを open できます。
* `open M in ...` という構文で局所的にモジュールを open できます。
* `M.( ... )` という構文で局所的にモジュールを open できます。

```{.satysfi eval="type-check-only"}
module ExampleModule = struct
  let foo = 42
  let bar n = n + 42
end

% 以下の 2 つは let baz = ExampleModule.bar ExampleModule.foo と等価です
let _ =
  open ExampleModule in
  bar foo
let _ = ExampleModule.(bar foo)

% 以下のようにも書けます (これより下で ExampleModule がすべて open されます)
open ExampleModule
let _ = bar foo
```

</div>

## 次に読みたい

→ [シグネチャ](programming-signature.html)
