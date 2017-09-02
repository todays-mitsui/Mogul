{-# LANGUAGE OverloadedStrings #-}

module Expr where

import Control.Monad (guard)
import Data.ByteString.Char8 (ByteString)
import Data.Set (Set)
import qualified Data.Set      as Set
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map

import Data
import Focus


-- | 式中の全ての定義済み関数を展開し、すべてのλ抽象をSKコンビネーターに展開する
compile :: Context -> Expr -> Expr
compile context x@(Var v)
  | v `Map.member` context  = compile context $ subst context x
  | otherwise               = x
compile context l@(_ :^ _)  = compile context $ unlambda l
compile context (e :$ e')   = compile context e :$ compile context e'

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
                           = let Func _ e = context ! v
                             in  subst context e
  | otherwise              = x
subst' vs context (v :^ e)  = v :^ subst' (v `Set.insert` vs) context e
subst' vs context (e :$ e') = subst' vs context e :$ subst' vs context e'

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- eval :: Context -> ExprFocus -> Maybe ExprFocus
-- eval context (Var v, argCount, crumbs) = do
--   rank <- varRank context v
--   e    <- varAlias context v
--   guard (rank >= argCount)
--   goUps rank (e, argCount, crumbs)
--
-- eval context

canReduct :: Context -> Int -> Expr -> Bool
canReduct context argCount (Var v)
  = case varRank context v of
         Just rank -> rank >= argCount
         Nothing   -> False

canReduct context argCount (_ :^ _ )
  = argCount >= 1

canReduct context argCount (e :$ e')
  = canReduct context (argCount+1) e || canReduct context 0 e'

--------------------------------------------------------------------------------

-- | 変数のランクを取得
varRank :: Context -> Ident -> Maybe Int
varRank context v = length . args <$> v `Map.lookup` context

-- | 変数の実体を取得
varAlias :: Context -> Ident -> Maybe Expr
varAlias context v = body <$> v `Map.lookup` context

-- | 式の書き換えを実行 (^x.M)N => M[x:=N]
rewrite :: Ident -> Expr -> Expr -> Expr
rewrite v e w@(Var w')
  | v == w'   = e
  | otherwise = w
rewrite v e (v' :^ e' ) = v' :^ rewrite v e e'
rewrite v e (e' :$ e'') = rewrite v e e' :$ rewrite v e e''
