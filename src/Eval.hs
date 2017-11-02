{-# LANGUAGE OverloadedStrings #-}

module Eval where


import Data.Text                  (Text)
import qualified Data.Text     as T
import Data.Set                   (Set)
import qualified Data.Set      as Set
import Data.Map.Lazy              (Map, (!), member, notMember)
import qualified Data.Map.Lazy as Map
import Control.Monad.State.Lazy

import Data


data BreadCrumb = LeftExpr Expr
                | Args [Expr]
  deriving (Eq, Show)

normalize :: [BreadCrumb] -> [BreadCrumb]
normalize (Args [] : bcs)             = bcs
normalize (Args es1 : Args es2 : bcs) = (Args (es1 ++ es2) : bcs)
normalize bcs                         = bcs

--------------------------------------------------------------------------------

eval context e = map uncrumb $ reduce context [] e

uncrumb :: (Expr, [BreadCrumb]) -> Expr
uncrumb (e, [])                = e
uncrumb (e, LeftExpr el : bcs) = uncrumb (el :$ e, bcs)
uncrumb (e, Args es     : bcs) = uncrumb (foldr (flip (:$)) e es, bcs)

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

foo = (Ident "x" :^ Var (Ident "x")) :$ Com (Ident"y")
c = Map.empty

-- eval2 context e = runState (eval1 e) (context, [Com (Ident "y")])

-- eval1 :: Expr -> State (Context, [Expr]) [Expr]
-- eval1 (Var _)  = return []
-- eval1 (x :^ e) = do
--   (_, es) <- get
--   case es of
--        []      -> return []
--        (e':es) -> return [rewrite x e' e]


-- eval :: Context -> [Expr] -> Expr -> [Expr]
-- eval context _      (Var _)    = []
-- eval context []     (x  :^ e') = []
-- eval context (e:es) (x  :^ e') = [rewrite x e e']
-- eval context es     (el :$ er) = concat [
--     map (:$ er) (eval context (er:es) el)
--   , map (el :$) (eval context [] er) ]
-- eval context es     (Com x)
--   | x `notMember` context      = []
--   | length es < arity f        = []
--   | otherwise                  = [foldr (\(v, e) cont -> rewrite v e cont) body (zip vs es)]
--   where
--     Just f = x `Map.lookup` context
--     vs   = params f
--     body = bareExpr f


rewrites :: [(Ident, Expr)] -> Expr -> Expr
rewrites args body = foldr (\(x, e) cont -> rewrite x e cont) body args

rewrite :: Ident -> Expr -> Expr -> Expr
rewrite x e v@(Var y)
  | x == y              = e
  | otherwise           = v
rewrite _ _ c@(Com _)   = c
rewrite x e (el :$ er)  = rewrite x e el :$ rewrite x e er
rewrite x e l@(y :^ e')
  | x == y              = l
  | otherwise           = y :^ rewrite x e e'



-- evals :: Context -> Expr -> [Expr]
-- evals context e = evals' [] context (e, 0, [])

-- evals' :: [Expr] -> Context -> ExprFocus -> [Expr]
-- evals' acc context focus =
--   case eval context focus of
--     Just focus' -> evals' (rootExpr focus' : acc) context focus'
--     Nothing     -> acc

-- eval :: Context -> ExprFocus -> Maybe ExprFocus
-- eval context   (Var _         , _    , _  ) = Nothing
-- eval context   (_ :^ _        , 0    , _  ) = Nothing
-- eval context f@(_ :^ _        , depth, _  ) = eval context =<< goUp f
-- eval context   ((v :^ e) :$ e', depth, bcs) = Just (apply v e e', depth, bcs)
-- eval context f@(Var v :$ e', depth, bcs) =
--   case (rank_ context v, getAlias context v) of
--        (Just n, Just e) -> if n <= depth+1
--                               then eval context =<< goUps (n-1) (e :$ e', depth, bcs)
--                               else eval context =<< goRight f
--        (Nothing, Nothing) -> Nothing
-- eval context f@(_ :$ _    , depth, bcs) = eval context =<< goLeft f

-- -- -- | Beta正規形かどうか判定する
-- -- -- | Beta Normal Form
-- -- isBetaNF :: Context -> ExprFocus
-- -- isBetaNF _       (Var _)           = True
-- -- isBetaNF _       (_ :^ _)          = True
-- -- isBetaNF _       (l@(_ :^ _) :$ _) = False
-- -- isBetaNF context (e :$ e')         = undefined

-- rank_ :: Context -> Ident -> Maybe Int
-- rank_ context v = arity <$> v `Map.lookup` context

-- getAlias :: Context -> Ident -> Maybe Expr
-- getAlias context v
--   | v `Map.member` context = let f = context ! v
--                              in  Just $ body f
--   | otherwise              = Nothing

-- apply :: Ident -> Expr -> Expr -> Expr
-- apply v (Var w) e
--   | v == w    = e
--   | otherwise = Var w
-- apply v (v' :^ e)  e' = v' :^ apply v e e'
-- apply v (e' :$ e'') e = apply v e' e :$ apply v e'' e
