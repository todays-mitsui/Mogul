{-# LANGUAGE OverloadedStrings #-}

module Data (
  Ident(..),
  isUniIdent,
  isLargeIdent,

  Expr(..),
  var,

  Func(..),

  Context,
  emptyContext,

  s, k, i
 ) where

import Data.Char               (isLower)
import Data.Text               (Text)
import qualified Data.Text     as T
import Data.Set                (Set)
import qualified Data.Set      as Set
import Data.Map.Lazy           (Map, (!))
import qualified Data.Map.Lazy as Map

-- | 識別子
-- |
-- | 有効な識別子は英小文字1文字から成るもの (ex. a,b,..,z)
-- | または、英大文字,アンダースコア(_),数字(0..9) の1文字以上の列
-- | ただし、有効な識別子のみ生成されるよう保証するのは Parser の役割とする
data Ident = Ident Text
             deriving (Eq, Ord, Show, Read)

isUniIdent :: Ident -> Bool
isUniIdent (Ident x) = (T.length x == 1) && (isLower . T.head $ x)
  where lowers = "abcdefghijklmnopqrstuvwxyz"

isLargeIdent :: Ident -> Bool
isLargeIdent = not . isUniIdent

--------------------------------------------------------------------------------

-- | λ式
data Expr = Expr :$ Expr
            | Ident :^ Expr
            | Var Ident
            deriving (Eq, Show, Read)

infixl 9 :$
infixr 7 :^

var :: Text -> Expr
var = Var . Ident

-- | 引数の長さを保持した無名関数
data Func = Func {
              args :: [Ident],
              body :: Expr
            }
            deriving (Eq, Show, Read)

type Context = Map Ident Func

emptyContext = Map.empty

--------------------------------------------------------------------------------

i :: Expr
i = var "i"

k :: Expr
k = var "k"

s :: Expr
s = var "s"
