# Mogul

skiコンビネーターをステップ実行する評価器です。

## セットアップ & 実行

[Stack](https://docs.haskellstack.org/en/stable/README/) コマンドが使える状態で以下のコマンドを実行すれば多分セットアップできます。

```console
$ git clone https://github.com/todays-mitsui/Mogul.git
$ cd Mogul
$ stack setup
$ stack build
$
$ #実行
$ stack exec mogul
```

## 文法

Unlambda に似た文法を持っています。

```
# 識別子
<lower>            ::= "a" | ... | "z"
<upper>            ::= "A" | ... | "Z"
<digit>            ::= "0" | ... | "9"
<identifier>       ::= <lower> | (<upper> | <digit> | "_")+

# λ式
<var>              ::= <identifier>
<lambda>           ::= "^" (<identifier>)+ "." <expr>
<apply>            ::= "`" <expr> <expr>
<expr>             ::= <var> | <lambda> | <apply>

# 関数定義
<func_name>        ::= <identifier>
<param>            ::= <identifier>
<ident_and_params> ::= <func_name> | "`" <ident_and_params> <param>
<def>              ::= <ident_and_params> "=" <expr> EOL

# コンテキスト (関数定義の組)
<context>          ::= (<def>)*
```
