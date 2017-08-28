# SKI-Mogul

skiコンビネータ−をステップ評価するインタプリタです。

## セットアップ

[Stack](https://docs.haskellstack.org/en/stable/README/) コマンドが使える状態で以下のコマンドを実行すれば多分セットアップできます。

```console
$ git clone https://github.com/todays-mitsui/ski-mogul.git
$ cd ski-mogul
$ stack setup
$ stack build
$
$ #実行
$ stack exec mogul-exe
```

## TODO

多分バグがあります。

- α簡約を全く考慮していないので何か実装する
- 関数呼び出しの中で関数呼び出しがネストしていると挙動が不定になる

プルリクエストお待ちしています。
