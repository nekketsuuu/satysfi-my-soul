# レポートのひな型

## stdjareport を使う方法

下の例は、まるまるコピー＆ペーストして組版することができます。

<div class="large-result">

```satysfi
@require: stdjareport

document (|
  title = {二次方程式の解};
  author = {\@nekketsuuu};
|) '<
  +p {
    このレポートでは，二次方程式${ax^2 + bx + c = 0}（ただし ${a \neq 0}）の
    一般的な解法をあたえる．
    二次方程式は平方完成することによって一般解を得ることができる．
    まず両辺を${a}で割る．
  }
  +math (${
    x^2 + \frac{b}{a} x + \frac{c}{a} = 0
  });
  +p {
    次に平方完成する．
  }
  +math (${
    \paren{x + \frac{b}{2a}}^2 - \frac{b^2}{4a^2} + \frac{c}{a} = 0
  });
  +p {
    あとは移項したり両辺平方根をとったりすることを繰り返す．
  }
  +align [
    [${\paren{x + \frac{b}{2a}}^2}; ${= \frac{b^2 - 4ac}{4a^2}}];
    [${x + \frac{b}{2a}}; ${= \pm \sqrt{\frac{b^2 - 4ac}{4a^2}}}];
    [${x}; ${= - \frac{b}{2a} \pm \frac{\sqrt{b^2 - 4ac}}{2a}}];
  ];
  +p {
    以上より，${x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}}であると分かった．
  }
>
```

</div>

## 他のひな型

* [ひな型](index.html#-1)
