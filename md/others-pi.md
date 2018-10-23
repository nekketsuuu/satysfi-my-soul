# 円周率の計算

```satysfi
@require: stdja
@require: standalone

let-rec repeat f n x =
  if n <= 0 then x
  else repeat f (n - 1) (f x)

let-rec sqrt x =
  % Babylonian method
  let x0 = 1.0 +. ((x -. 1.0) *. 0.5) in
  let aux a = 0.5 *. (a +. x /. a) in
  repeat aux 20 x0

let pi =
  % Gauss-Legendre algorithm
  let aux (a, b, t, p) =
    let next-a = (a +. b) *. 0.5 in
    let next-b = sqrt (a *. b) in
    let sub = a -. next-a in
    let next-t = t -. p *. sub *. sub in
    let next-p = 2.0 *. p in
    (next-a, next-b, next-t, next-p)
  in
  let (a, b, t, _) = repeat aux 3 (1.0, 0.70710678118, 0.25, 1.0) in
  let ab = a +. b in
  0.25 *. ab *. ab /. t

let-inline \show-float x = embed-string (show-float x)

in
standalone '<
  +p {
    \show-float(pi);
  }
>
```

## 関連

* もっとすごいもの: [今日はアレの日！](http://d.hatena.ne.jp/zrbabbler/20180314/1520982097) -- マクロツイーター
