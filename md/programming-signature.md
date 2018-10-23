# シグネチャ

このページでは SATySFi におけるシグネチャについて解説します。

<div class="box-note" markdown="1">

**注意**: **SATySFi のモジュールに関する公式ドキュメントは執筆時点ではまだ無いので、このページに書かれていることは変更される可能性があります。** SATySFi version 0.0.3 時点では OCaml のモジュールと構文が似ているので、OCaml のマニュアル ["Chapter 2  The module system"](https://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html) や M.Hiroi さんの[「お気楽 OCaml プログラミング入門 モジュール」](http://www.geocities.jp/m_hiroi/func/ocaml09.html)もある程度参考になります。

</div>

**先に[モジュール](programming-module.html)の知識を仮定します。**

## シグネチャの付け方

モジュールに「型」を付けることによって、モジュール外からアクセスできる変数を制限することができます。この「型」のことをシグネチャと呼びます。シグネチャは変数名と型のペアの羅列を使ってモジュールの使い方 (インターフェース) を提供しているとも言えます。

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

**中級者向け**: SATySFi version 0.0.3 では現状、OCaml にあるようなシグネチャだけの定義 `module type 名前 = sig ... end` はできません。

</div>

### シグネチャによる抽象化の例

具体例として、自然数を表すモジュール `Nat` を作ろうとしてみましょう。

```{.satysfi eval="type-check-only"}
module Nat : sig
  type t
  val zero : t
  val succ : t -> t
end = struct
  type t = int
  let zero = 0
  let succ n = n + 1
end
```

このモジュールでは自然数の型として `t` を定義していますが、シグネチャには `type t` とだけ書かれており、型の "実装" が隠蔽されています。したがって次のように書くとエラーが出ます。モジュール `Nat` の内部では int 型の値が使われていますが、その情報は外部に公開されていないのです。

```{.satysfi eval="error"}
module Nat : sig
  type t
  val zero : t
  val succ : t -> t
end = struct
  type t = int
  let zero = 0
  let succ n = n + 1
end

%% BEGIN
let one = Nat.succ 0
%% END
```

以下のように書くと大丈夫です。

```{.satysfi eval="type-check-only"}
module Nat : sig
  type t
  val zero : t
  val succ : t -> t
end = struct
  type t = int
  let zero = 0
  let succ n = n + 1
end

%% BEGIN
let one = Nat.succ Nat.zero
%% END
```

シグネチャによる抽象化を確かめてみるために、シグネチャは変えないまま、実装だけ変えてみます。以下のコードではモジュール内部の実装は変わっていますが、外部の実装は変わっていません。

```{.satysfi eval="type-check-only"}
module Nat : sig
  type t
  val zero : t
  val succ : t -> t
end = struct
  type t = Z | S of t
  let zero = Z
  let succ n = S(n)
end

let one = Nat.succ Nat.zero
```

## シグネチャ内での構文

以下の表は、シグネチャ内で使える構文を簡易的に示したものです。

| モジュール | シグネチャ |
|:-----------|:-----------|
| `let f = 中身` | `val f : 型` |
| `type t = 中身` | `type t = 中身` または `type t` (意味は変わります) |

`let-rec f = 中身` や `let-inline f = 中身` 等も `val f : 型` になります。また、コマンド系の変数には下で説明している `direct f : 型` も使えます。

## シグネチャにおける direct

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

`direct` を使わない場合、次のように `\M.id` 形式や `+M.id` 形式で書くことになります。何らかの事情で同じ名前のコマンドを複数作りたいときはこちらを採用することになるでしょう。

```{.satysfi eval="type-check-only"}
@require: stdja
@require: standalone

module My : sig
  val \name : [] inline-cmd
end = struct
  let-inline \name = embed-string `@nekketsuuu`
end

in
standalone '<
  +p {
    I am \My.name;.
  }
>
```
