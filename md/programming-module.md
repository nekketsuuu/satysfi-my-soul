# モジュール

このページでは、SATySFi で使える「モジュール」という機能について解説します。

<div class="box-note" markdown="1">

**メモ**: **SATySFi のモジュールに関する公式ドキュメントは執筆時点ではまだ無いので、このページに書かれていることは変更される可能性があります。** SATySFi version 0.0.3 時点では OCaml のモジュールと構文が似ているので、OCaml のマニュアル ["Chapter 2  The module system"](https://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html) や M.Hiroi さんの[「お気楽 OCaml プログラミング入門 モジュール」](http://www.geocities.jp/m_hiroi/func/ocaml09.html)もある程度参考になります。

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

モジュール `M` 内にある `foo` という変数には `M.foo` とピリオドでつなぐことでアクセスできます。

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

**中級者向け**: OCaml にある「モジュールの `open`」に相当する機能は実装されていません。[Issue #79](https://github.com/gfngfn/SATySFi/issues/79) をご覧ください。

</div>

<div class="box-note" markdown="1">

**中級者向け**: `M.( ... )` という構文で局所的にモジュールを open することができます。

```{.satysfi eval="type-check-only"}
module ExampleModule = struct
  let foo = 42
  let bar n = n + 42
end

% 以下は let baz = ExampleModule.bar ExampleModule.foo と等価です
let baz = ExampleModule.(bar foo)
```

</div>

## シグネチャ

モジュールに「型」を付けることによって、モジュール外からアクセスできる変数を制限することができます。この「型」のことをシグネチャと呼びます。シグネチャは変数名と型のペアの羅列でモジュールの使い方 (インターフェース) を提供しているとも言えます。

シグネチャは `module 名前 : sig インターフェース end = struct 定義列 end` という記法でつけることができます。

```{.satysfi eval="type-check-only"}
module ExampleModule : sig
  val foo : int
  val bar : int -> int
end = struct
  let foo = 42
  let bar n = n + 42

  % 以下の baz はモジュール外から参照できません
  let baz = 108
end
```

上の例で、モジュール外から baz を参照しようとするとエラーが出ます。

```{.satysfi eval="error"}
module ExampleModule : sig
  val foo : int
  val bar : int -> int
end = struct
  let foo = 42
  let bar n = n + 42

  % 以下の baz はモジュール外から参照できません
  let baz = 108
end

let-inline \show-int n =
  embed-string (arabic n)
in
standalone '<
%% BEGIN
% baz はシグネチャに書かれていないので外部に公開されていません
+p {
  baz = \show-int(ExampleModule.baz);
}
%% END
>
```

<div class="box-note" markdown="1">

**中級者向けメモ**: SATySFi version 0.0.3 では現状、OCaml にあるようなシグネチャだけの定義 `module type = sig ... end` はできません。

</div>

### シグネチャ内での構文

以下の表は、シグネチャ内で使える構文を簡易的に示したものです。

| モジュール | シグネチャ |
|:-----------|:-----------|
| `let f = 中身` | `val f : 型` |
| `type t = 中身` | `type t = 中身` (`type t` とだけ書くこともできます) |

## モジュールと direct

インラインコマンドやブロックコマンド、数式コマンドなどのコマンドは、モジュール内でそのまま宣言していると、使うたびにモジュール名を付けないといけなくなり面倒です。シグネチャにおいて `val` の代わりに `direct` を使うと、グローバルな名前空間において宣言することができます。

```satysfi
@require: stdja
@require: standalone

module My : sig
  direct \name : [] inline-cmd
end = struct
  let-inline \name = embed-string `@nekketsuuu`
end

in
standalone '<
  +p {
    I am \name;.
  }
>
```

`direct` を使わない場合、次のように `\M.id` 形式や `+M.id` 形式で書くことになります。

```{.satysfi eval="type-check-only"}
@require: stdja
@require: standalone

module My : sig
  val \name : [] inline-cmd
end = struct
  let-inline \name = embed-string `\nekketsuuu`
end

in
standalone '<
  +p {
    I am \My.name;.
  }
>
```
