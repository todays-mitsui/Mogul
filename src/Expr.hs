{-# LANGUAGE OverloadedStrings #-}

module Expr where

import Control.Monad (guard)

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Set      as Set
import Data.Set (Set, union, insert)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map, (!))
import Text.Parsec hiding (token)
import Text.Parsec.Text

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
varRank context v = arity <$> v `Map.lookup` context

-- | 変数の実体を取得
varAlias :: Context -> Ident -> Maybe Expr
varAlias context v = body <$> v `Map.lookup` context

--------------------------------------------------------------------------------

rewrite :: Ident -> Expr -> Expr -> Expr
-- rewrite v er el = let (el', er') = sanitize (el, er)
--                   in  rewrite' v el' er'
rewrite = rewrite'

-- | 式の書き換えを実行 (^x.M)N => M[x:=N]
rewrite' :: Ident -> Expr -> Expr -> Expr
rewrite' v e w@(Var w')
  | v == w'   = e
  | otherwise = w
rewrite' v e (v' :^ e' ) = v' :^ rewrite' v e e'
rewrite' v e (e' :$ e'') = rewrite' v e e' :$ rewrite' v e e''

-- sanitize :: (Expr, Expr) -> (Expr, Expr)
-- sanitize (el, er) = let freel = collectFreeVars el
--                         freer = collectFreeVars er
--                     in  (alphaConv freer el, alphaConv freel er)

--------------------------------------------------------------------------------

-- | 定義済みの関数に規定数の引数を適用して評価する
-- | TODO: α変換を考慮する
apply :: Context -> Ident -> [Expr] -> Expr
apply context v es = case v `Map.lookup` context of
                            Just (Func args funcBody) -> undefined -- apply' args es funcBody
                            Nothing                   -> foldl (:$) (Var v) es

apply' :: [Ident] -> [Expr] -> Expr -> Expr
apply' args es funcBody = foldl (\body (arg, e) -> rewrite arg e body) funcBody $ zip args es

--------------------------------------------------------------------------------

-- rename :: Set Ident -> Ident -> Ident
-- rename reserved v
--   | not $ v `Set.member` reserved = v
--   | otherwise                     = rename reserved $ rename' v
--
-- rename' :: Ident -> Ident
-- rename' (UniIdent   s)          = LargeIdent (T.toUpper s) $ Just 0
-- rename' (LargeIdent s Nothing)  = LargeIdent s             $ Just 0
-- rename' (LargeIdent s (Just n)) = LargeIdent s             $ Just (n + 1)

--------------------------------------------------------------------------------

-- alphaConv :: Set Ident -> Expr -> Expr
-- alphaConv prohibitedVars e = let reservedVars = prohibitedVars `union `collectVars e
--                              in  alphaConv' prohibitedVars reservedVars e
--
-- alphaConv' :: Set Ident -> Set Ident -> Expr -> Expr
-- alphaConv' prohibitedVars reservedVars (v :^ e)
--   | v `Set.member` prohibitedVars = let v' = rename reservedVars v
--                                     in  v' :^ alphaConv' prohibitedVars reservedVars (replace v v' e)
--   | otherwise                     = v :^ alphaConv' prohibitedVars reservedVars e
-- alphaConv' prohibitedVars reservedVars (e :$ e')
--   = alphaConv' prohibitedVars reservedVars e :$ alphaConv' prohibitedVars reservedVars e'
-- alphaConv' _ _ v@(Var _) = v
--
-- replace :: Ident -> Ident -> Expr -> Expr
-- replace v v'   (e   :$ e') = replace v v' e :$ replace v v' e
-- replace v v' l@(v'' :^ e)
--   | v == v''  = l
--   | otherwise = v'' :^ replace v v' e
-- replace v v' (Var v'')
--   | v == v''  = Var v'
--   | otherwise = Var v''

--------------------------------------------------------------------------------

-- | 式中の変数を全て列挙する
collectVars :: Expr -> Set Ident
collectVars (Var v)   = Set.singleton v
collectVars (e :$ e') = collectVars e `union` collectVars e'
collectVars (v :^ e)  = collectVars e

-- | 式中の自由変数を全て列挙する
collectFreeVars :: Expr -> Set Ident
collectFreeVars = collectFreeVars' Set.empty

collectFreeVars' :: Set Ident -> Expr -> Set Ident
collectFreeVars' vs (Var v)
  | v `Set.member` vs         = Set.empty
  | otherwise                 = Set.singleton v
collectFreeVars' vs (e :$ e') = collectFreeVars' vs e `union` collectFreeVars' vs e'
collectFreeVars' vs (v :^ e)  = collectFreeVars' (v `insert` vs) e
