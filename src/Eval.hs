{-# LANGUAGE OverloadedStrings #-}

module Eval
    ( evals, eval
    , evalsPlus, evalPlus
    , Nav(..)
    ) where


import Data.Text                  (Text)
import qualified Data.Text     as T
import Data.Set                   (Set)
import qualified Data.Set      as Set
import Data.Map.Lazy              (Map, (!), member, notMember)
import qualified Data.Map.Lazy as Map
import Control.Monad.State.Lazy

import Data


-- | λ式を段階的に簡約して、すべてのステップの式を返す
-- 評価戦略は最左最外簡約
evals :: Context -> Expr -> [Expr]
evals context e = case eval context e of
                       []       -> []
                       (e' : _) -> e' : evals context e'

-- | λ式をβ簡約した結果の式すべてを返す
-- | ただしβ簡約は必ず一か所だけ
eval :: Context -> Expr -> [Expr]
eval context e = map uncrumb $ reduce context [] e

--------------------------------------------------------------------------------

evalsPlus :: Context -> Expr -> [(Expr, [Nav])]
evalsPlus context e = shift e $ evalsPlus' context e

shift :: Expr -> [(Expr, [Nav])] -> [(Expr, [Nav])]
shift e []             = [(e, [])]
shift e ((e', ns):pairs) = (e, ns) : shift e' pairs

evalsPlus' :: Context -> Expr -> [(Expr, [Nav])]
evalsPlus' context e = case evalPlus context e of
                            []             -> []
                            ((e', ns) : _) -> (e', ns) : evalsPlus' context e'

evalPlus :: Context -> Expr -> [(Expr, [Nav])]
evalPlus context e = [ (e'', ns) | (e', bcs) <- reduce context [] e, let e'' = uncrumb (e', bcs), let ns = navs bcs ]

--------------------------------------------------------------------------------

-- | 構文木を辿っていくとき、着目していない側の部分木を保持しておく構造体
-- 着目している部分木と着目していない部分木で構文木全体を常に表現できるように保つ
--
-- (el :$ er, breadcrumbs) === (er, LeftExpr el : breadcrumbs)
-- (el :$ er, breadcrumbs) === (el, Args [er] : breadcrumbs)
-- 逆も然り、
-- (e, LeftExpr el            : breadcrumbs) === (el :$ e, breadcrumbs)
-- (e, Args [e1, e2, ..., en] : breadcrumbs) === (e :$ e1 :$ e2 :$ ... :$ en, breadcrumbs)
data BreadCrumb = LeftExpr Expr  -- ^ el :$ er の er に着目しているとき el を保持する
                | Args [Expr]    -- ^ el :$ er の el に着目しているとき er を保持する
  deriving (Eq, Show)

-- | 不正な [BreadCrumb] を修正して正規化する
normalize :: [BreadCrumb] -> [BreadCrumb]
normalize (Args [] : bcs)             = bcs
normalize (Args es1 : Args es2 : bcs) = (Args (es1 ++ es2) : bcs)
normalize bcs                         = bcs

uncrumb :: (Expr, [BreadCrumb]) -> Expr
uncrumb (e, [])                = e
uncrumb (e, LeftExpr el : bcs) = uncrumb (el :$ e, bcs)
uncrumb (e, Args es     : bcs) = uncrumb (foldl (:$) e es, bcs)


data Nav = NavLeft | NavRight
  deriving (Eq, Show)

navs :: [BreadCrumb] -> [Nav]
navs = concatMap crumb2navs . reverse

crumb2navs :: BreadCrumb -> [Nav]
crumb2navs (LeftExpr _) = [NavRight]
crumb2navs (Args es)    = map (const NavLeft) es

--------------------------------------------------------------------------------

-- | 構文木を辿り、簡約可能な個所を探して一か所だけ簡約した結果のすべてを返す
-- 簡約可能な個所が n か所あれば、結果は長さ n のリストになる
reduce :: Context -> [BreadCrumb] -> Expr -> [(Expr, [BreadCrumb])]
reduce _ (Args (e:es) : bcs) (x :^ e') = [(rewrite x e e', Args es : bcs)]
reduce context [] (el :$ er) = concat [
    reduce context [Args [er]]   el
  , reduce context [LeftExpr el] er
  ]
reduce context bcs (el :$ er) = concat [
    reduce context (normalize (Args [er] : bcs)) el
  , reduce context (LeftExpr el : bcs)           er
  ]
reduce context (Args es : bcs) (Com x)
  | x `notMember` context      = []
  | length es < arity f        = []
  | otherwise                  = [(rewrites (zip vs es) body, normalize (Args (drop (arity f) es) : bcs))]
  where
    Just f = x `Map.lookup` context
    vs   = params f
    body = bareExpr f
reduce _ _ _ = []


-- | 式の中の仮引数を別の式で置き換えた式を返す
-- | 複数のβ簡約を一度に行うための関数
rewrites :: [(Ident, Expr)] -> Expr -> Expr
rewrites args body = foldr (\(x, e) cont -> rewrite x e cont) body args

-- | 式の中の仮引数を別の式で置き換えた式を返す
-- | β簡約: `(^x.M)N => M[x:=N] に相当する
rewrite :: Ident -> Expr -> Expr -> Expr
rewrite x e v@(Var y)
  | x == y              = e
  | otherwise           = v
rewrite _ _ c@(Com _)   = c
rewrite x e (el :$ er)  = rewrite x e el :$ rewrite x e er
rewrite x e l@(y :^ e')
  | x == y              = l
  | otherwise           = y :^ rewrite x e e'
