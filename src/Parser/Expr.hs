module Parser.Expr
    ( parseExpr
    , parseContext
    , subst, substF
    , expr
    , ident
    , def
    ) where


import Control.Monad (void)
import Control.Applicative hiding ((<|>), many)

import qualified Data.Set      as Set
import Data.Set (Set, insert, member, fromList)

import qualified Data.Map.Lazy as Map
import qualified Data.Text     as T
import Data.Text (Text, pack, singleton)

import Text.Parsec hiding (token)
import Text.Parsec.Text

import Data (Ident(..), Expr(..), Func(..), Context)
import qualified Data as D


parseExpr :: Text -> Either ParseError Expr
parseExpr src = subst <$> parse expr "" src

parseContext :: Text -> Either ParseError Context
parseContext src = parse context "" src <$$> \context ->
                       substF <$> context
  where
    xs <$$> f = f <$> xs


subst :: Expr -> Expr
subst = subst' Set.empty

subst' :: Set Ident -> Expr -> Expr
subst' vs (Var x)
  | x `Set.member` vs = Var x
  | otherwise         = Com x
subst' vs (Com x)
  | x `member` vs = Var x
  | otherwise         = Com x
subst' vs (el :$ er)  = subst' vs el :$ subst' vs er
subst' vs (x  :^ e)   = let vs' = x `insert` vs
                            e'  = subst' vs' e
                        in  x :^ e'

substF :: Func -> Func
substF (Func args bareExpr) = let e = subst' (fromList args) bareExpr
                              in  Func args e

--------------------------------------------------------------------------------

-- | Mogul syntax
--
-- | 識別子
-- <lower>            ::= "a" | ... | "z"
-- <upper>            ::= "A" | ... | "Z"
-- <digit>            ::= "0" | ... | "9"
-- <identifier>       ::= <lower> | (<upper> | <digit> | "_")+
--
-- | λ式
-- <var>              ::= <identifier>
-- <lambda>           ::= "^" (<identifier>)+ "." <expr>
-- <apply>            ::= "`" <expr> <expr>
-- <expr>             ::= <var> | <lambda> | <apply>
--
-- | 関数定義
-- <func_name>        ::= <identifier>
-- <param>            ::= <identifier>
-- <ident_and_params> ::= <func_name> | "`" <ident_and_params> <param>
-- <def>              ::= <ident_and_params> "=" <expr> EOL
--
-- | コンテキスト (関数定義の組)
-- <context>          ::= (<def>)*

--------------------------------------------------------------------------------

-- | 識別子
ident :: Parser Ident
ident = ident' <|> ident''

ident' :: Parser Ident
ident' = Ident . singleton <$> lower

ident'' :: Parser Ident
ident'' = Ident . pack <$> many1 (upper <|> digit <|> char '_')

--------------------------------------------------------------------------------

-- | λ式
expr :: Parser Expr
expr = apply <|> lambda <|> var

-- | 変数
var :: Parser Expr
var = Var <$> ident

-- | 関数抽象
lambda :: Parser Expr
lambda = do
    token $ char '^'
    v  <- token ident
    vs <- many $ token ident
    token $ char '.'
    e  <- token expr
    return $ mkLambda (v:vs) e
  where
    mkLambda vs e = foldr (:^) e vs

-- | 関数適用
apply :: Parser Expr
apply = do
    token $ char '`'
    e  <- token expr
    e' <- token expr
    return $ e :$ e'

--------------------------------------------------------------------------------

-- | 関数定義の左辺部 "```f x y z" の形だけを許す
defFunc :: Parser (Ident, [Ident])
defFunc = defFunc' <|> do
    funcName <- token ident
    return (funcName, [])

defFunc' :: Parser (Ident, [Ident])
defFunc' = do
    token $ char '`'
    (funcName, args) <- token defFunc
    arg <- token ident
    return (funcName, arg:args)

-- | 関数定義
def :: Parser (Ident, Func)
def = do
    (funcName, reversedArgs) <- token defFunc
    token $ char '='
    e <- token expr
    spaces'
    skipMany lineComment
    void endOfLine <|> eof
    return (funcName, Func (reverse reversedArgs) e)

-- | コンテキスト (関数定義の組)
context :: Parser Context
context = Map.fromList <$> many1 def

--------------------------------------------------------------------------------

-- | パーサーの前の空白, 空行, 行コメントを読み飛ばすような新しいパーサーを返す
token :: Parser a -> Parser a
token p = spaces >> skipMany (lineComment >> endOfLine) >> spaces >> p

-- | 行コメント
lineComment :: Parser ()
lineComment = char '#' >> skipMany (noneOf "\n\r") >> lookAhead (void endOfLine <|> eof) <?> "line comment"

-- | \t, \r を除く非印字文字
space' :: Parser Char
space' = oneOf " \t\v\f" <?> "space"

-- | 改行を許容しない white space の読み飛ばし
spaces' :: Parser ()
spaces' = skipMany space' <?> "white space"
