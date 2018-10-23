# 曜日-of-日付

SATySFi で[ツェラーの公式](https://ja.wikipedia.org/wiki/%E3%83%84%E3%82%A7%E3%83%A9%E3%83%BC%E3%81%AE%E5%85%AC%E5%BC%8F)を使って日付から曜日を計算するコードです。

```satysfi
@require: stdja
@require: standalone

% 日付と曜日を表す型の定義

type date = (|
  year  : int;
  month : int;
  date  : int;
|)

type day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

let day-of-int n =
  let h = n mod 7 in
  match h with
  | 1 -> Sun
  | 2 -> Mon
  | 3 -> Tue
  | 4 -> Wed
  | 5 -> Thu
  | 6 -> Fri
  | _ -> Sat

let string-of-day-ja d =
  match d with
  | Mon -> `月`
  | Tue -> `火`
  | Wed -> `水`
  | Thu -> `木`
  | Fri -> `金`
  | Sat -> `土`
  | Sun -> `日`

% ツェラーの公式による曜日計算
% ただし、グレゴリオ暦を仮定します
% y >= 1582 程度を仮定します

let get-day date =
  let y =
    let base =
      if date#month <= 2 then date#year - 1
      else date#year
    in
    if base >= 0 then base
    else 1 - base
  in
  let m =
    if date#month <= 2 then 12 + date#month
    else date#month
  in
  let d = date#date in

  let j = y / 100 in
  let yy = y mod 100 in
  let gamma = 5 * j + j / 4 in

  let i1 = (13 * (m + 1)) / 5 in
  let i2 = yy / 4 in
  let sum = d + i1 + yy + i2 + gamma in
  day-of-int sum

let-inline \day-ja date =
  get-day date
    |> string-of-day-ja
    |> embed-string

in
standalone '<
  +p {
    2018年7月10日は\day-ja(| year = 2018; month = 7; date = 10 |);曜日です。
  }
>
```
