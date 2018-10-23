# 正規表現

このページでは、SATySFi のライブラリ開発者向けに、SATySFi で使える正規表現について解説します。

<div class="box-note" markdown="1">

**注意**: **SATySFi の正規表現に関する公式ドキュメントは執筆時点ではまだ無いので、このページに書かれていることは変更される可能性があります。正規表現関係のプリミティブは Pull Request [#48](https://github.com/gfngfn/SATySFi/pull/48), [#56](https://github.com/gfngfn/SATySFi/pull/56) あたりで議論されています。重要な点として、2018 年 10 月現在まだマルチバイト文字列に対応していません。

</div>

## 正規表現を表す値の作成

`regexp-of-string : string -> regexp` プリミティブを使うと正規表現型の値を作ることができます。使える正規表現の構文は SATySFi version 0.0.3 においては OCaml の `Str.regexp` で使えるものと同じです。[こちらをご覧ください](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html)。

```{.satysfi eval="type-check-only"}
let re = regexp-of-string `nekketsu*`
```

## マッチ判定

`string-match : regexp -> string -> bool` プリミティブを使うと正規表現のマッチ判定ができます。

```{.satysfi eval="type-check-only"}
let pat = regexp-of-string `nekketsu*`
let _ = string-match pat `nekketsuuu`  % --> true
let _ = string-match pat `nekketuuuu`  % --> false
```

## 正規表現を使った文字列分割

`split-on-regexp : regexp -> string -> (int, string) list` が用意されています。`split-on-regexp pat str` で、`pat` で表現されたセパレーターで `str` を分割し、それぞれの文字列の最初の半角スペース列を取り除いて返します。返り値は整数と文字列のペアのリストになっており、整数は取り除かれた半角スペースの数です。

```satysfi
@require: stdja
@require: standalone

standalone '<
%% BEGIN
+p (
  let pat = regexp-of-string `,` in
  let show-result iss =
    let string-of-is (i, s) =
      `(` ^ (arabic i) ^ `, ` ^ s ^ `)`
    in
    let-rec aux iss =
      match iss with
      | [] -> ` `
      | is :: iss -> (string-of-is is) ^ `; ` ^ (aux iss)
    in
    embed-string (`[` ^ (aux iss) ^ `]`)
  in
  let it = show-result(split-on-regexp pat `aaa, bbb, ccc,  ddd, eee  , fff`) in
  {
    #it;
  }
);
%% END
>
```
