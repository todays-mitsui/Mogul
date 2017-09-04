module Parser (
  ident,
  expr,
  def,
  context,
  lineComment
) where

import Control.Monad (void)
import Control.Applicative hiding ((<|>), many)
import qualified Data.Map.Lazy as Map
import Data.ByteString.Char8 (ByteString, singleton, pack)
import Text.Parsec hiding (token)
import Text.Parsec.ByteString

import Data
-- import Expr


ident :: Parser Ident
ident = ident' <|> ident''

ident' :: Parser Ident
ident' = Ident . singleton <$> lower

ident'' :: Parser Ident
ident'' = Ident . pack <$> many1 (upper <|> digit <|> char '_')

--------------------------------------------------------------------------------

-- | 式
expr :: Parser Expr
expr = apply <|> lambda <|> var

-- | 関数適用
apply :: Parser Expr
apply = do
  token $ char '`'
  e  <- token expr
  e' <- token expr
  return $ e :$ e'

-- | λ抽象
lambda :: Parser Expr
lambda = do
  token $ char '^'
  v  <- token ident
  vs <- many $ token ident
  token $ char '.'
  e  <- token expr
  return $ mkLambda (v:vs) e

-- | 変数
var :: Parser Expr
var = Var <$> ident

--------------------------------------------------------------------------------

-- | 関数定義の左辺部 "```f x y z" の形だけを許す
defFunc :: Parser (Ident, [Ident])
defFunc = defFunc' <|> do
  v <- token ident
  return (v, [])

defFunc' :: Parser (Ident, [Ident])
defFunc' = do
  token $ char '`'
  (funcName, args) <- token defFunc
  arg <- token ident
  return (funcName, arg:args)

-- | 関数定義
def :: Parser (Ident, Func)
def = do
  (f, reversedArgs) <- token defFunc
  token $ char '='
  e <- token expr
  spaces'
  skipMany lineComment
  void endOfLine <|> eof
  return (f, Func (reverse reversedArgs) e)

-- | 関数定義の組
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

--------------------------------------------------------------------------------

mkLambda :: [Ident] -> Expr -> Expr
mkLambda vs e = foldr (\v expr -> v :^ expr) e vs
