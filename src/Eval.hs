{-# LANGUAGE OverloadedStrings #-}

module Eval where


import Data.Functor ((<$>))
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map

import Data
import Unlambda hiding (apply)
import Focus


evals :: Context -> Expr -> [Expr]
evals context e = evals' [] context (e, 0, [])

evals' :: [Expr] -> Context -> ExprFocus -> [Expr]
evals' acc context focus =
  case eval context focus of
    Just focus' -> evals' (rootExpr focus' : acc) context focus'
    Nothing     -> acc

eval :: Context -> ExprFocus -> Maybe ExprFocus
eval context   (Var _         , _    , _  ) = Nothing
eval context   (_ :^ _        , 0    , _  ) = Nothing
eval context f@(_ :^ _        , depth, _  ) = eval context =<< goUp f
eval context   ((v :^ e) :$ e', depth, bcs) = Just (apply v e e', depth, bcs)
eval context f@(Var v :$ e', depth, bcs) =
  case (rank_ context v, getAlias context v) of
       (Just n, Just e) -> if n <= depth+1
                              then eval context =<< goUps (n-1) (e :$ e', depth, bcs)
                              else eval context =<< goRight f
       (Nothing, Nothing) -> Nothing
eval context f@(_ :$ _    , depth, bcs) = eval context =<< goLeft f

-- -- | Beta正規形かどうか判定する
-- -- | Beta Normal Form
-- isBetaNF :: Context -> ExprFocus
-- isBetaNF _       (Var _)           = True
-- isBetaNF _       (_ :^ _)          = True
-- isBetaNF _       (l@(_ :^ _) :$ _) = False
-- isBetaNF context (e :$ e')         = undefined

rank_ :: Context -> Ident -> Maybe Int
rank_ context v = arity <$> v `Map.lookup` context

getAlias :: Context -> Ident -> Maybe Expr
getAlias context v
  | v `Map.member` context = let f = context ! v
                             in  Just $ body f
  | otherwise              = Nothing

apply :: Ident -> Expr -> Expr -> Expr
apply v (Var w) e
  | v == w    = e
  | otherwise = Var w
apply v (v' :^ e)  e' = v' :^ apply v e e'
apply v (e' :$ e'') e = apply v e' e :$ apply v e'' e
