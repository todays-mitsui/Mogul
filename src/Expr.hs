{-# LANGUAGE OverloadedStrings #-}

module Expr where

import Data.ByteString.Char8 (ByteString)
import Data.Set (Set)
import qualified Data.Set      as Set
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map


data Ident = Ident ByteString
             deriving (Eq, Ord, Show, Read)

infixl 9 :$
infixr 7 :^
data Expr = Expr :$ Expr
            | Ident :^ Expr
            | Var Ident
            deriving (Eq, Show, Read)

type Context = Map Ident Expr

emptyContext = Map.empty

--------------------------------------------------------------------------------

i :: Expr
i = Var (Ident "i")

k :: Expr
k = Var (Ident "k")

s :: Expr
s = Var (Ident "s")

--------------------------------------------------------------------------------

-- | 式中の全てのλ抽象をSKコンビネーターに展開する
unlambda :: Expr -> Expr
unlambda e@(Var _) = e
unlambda (e :$ e') = unlambda e :$ unlambda e'
unlambda (x :^ e)  = unlambda $ resolve x e


-- | λ式の中から変数を取り除く
resolve :: Ident -> Expr -> Expr
resolve x e@(Var y)
  -- ^x.x === i
  | x == y                = i
  -- ^x.y === `ky
  | otherwise             = k :$ e

resolve x e@(e' :$ e''@(Var y))
  -- ^x.M === `kM
  | not (x `isFreeIn` e)  = k :$ e
  -- ^x.`Mx === M
  | not (x `isFreeIn` e') = e'
  -- ^x.`MN === ``s ^x.M ^x.N
  | otherwise             = s :$ resolve x e' :$ resolve x e''

resolve x e@(e' :$ e'')
  -- ^x.M === `kM
  | not (x `isFreeIn` e)  = k :$ e
  -- ^x.`M N === ``s ^x.M ^x.N
  | otherwise             = s :$ resolve x e' :$ resolve x e''

resolve x e@(y :^ e')
  | x == y    = k :$ e
  | otherwise = resolve x $ resolve y e'

--------------------------------------------------------------------------------

-- | 変数が式の中に自由変数として含まれるかどうか判定する
isFreeIn :: Ident -> Expr -> Bool
x `isFreeIn` (Var y)
  | x == y             = True
  | otherwise          = False
x `isFreeIn` (y :^ e)
  | x == y             = False
  | otherwise          = x `isFreeIn` e
x `isFreeIn` (e :$ e') = x `isFreeIn` e || x `isFreeIn` e'

--------------------------------------------------------------------------------

-- | 式に含まれる定義済み変数を展開する
subst :: Context -> Expr -> Expr
subst = subst' Set.empty

subst' :: Set Ident -> Context -> Expr -> Expr
subst' vs context x@(Var v)
  | v `Set.notMember` vs && v `Map.member` context
                           = subst context $ context ! v
  | otherwise              = x
subst' vs context (v :^ e)  = v :^ subst' (v `Set.insert` vs) context e
subst' vs context (e :$ e') = subst' vs context e :$ subst' vs context e'

--------------------------------------------------------------------------------

compile :: Context -> Expr -> Expr
compile context x@(Var v)
  | v `Map.member` context  = compile context $ subst context x
  | otherwise               = x
compile context l@(_ :^ _)  = compile context $ unlambda l
compile context (e :$ e')   = compile context e :$ compile context e'
