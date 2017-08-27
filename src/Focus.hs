module Focus where


import Data.Functor ((<$>))

import Expr (Expr(..), Ident, s, k, i)


type Depth = Int

data ExprCrumb = RightCrumb Expr
               | LeftCrumb Depth Expr
               | LambdaCrumb Depth Ident
               deriving (Eq, Show, Read)

type ExprFocus = (Expr, Depth, [ExprCrumb])

--------------------------------------------------------------------------------

-- | 関数適用の左側(関数側)にフォーカスを移す
goLeft :: ExprFocus -> Maybe ExprFocus
goLeft ((e :$ e'), depth, bc) = Just (e, depth+1, RightCrumb e' : bc)
goLeft _                      = Nothing

-- | 関数適用の右側(引数側)にフォーカスを移す
goRight :: ExprFocus -> Maybe ExprFocus
goRight ((e :$ e'), depth, bc) = Just (e', 0, LeftCrumb depth e : bc)
goRight _                      = Nothing

--------------------------------------------------------------------------------

-- | 関数抽象の内側にフォーカスを移す
goIntoLambda :: ExprFocus -> Maybe ExprFocus
goIntoLambda ((v :^ e), depth, bc) = Just (e, 0, LambdaCrumb depth v : bc)
goIntoLambda _                     = Nothing

--------------------------------------------------------------------------------

-- | フォーカスを一つ外側に移す
goUp :: ExprFocus -> Maybe ExprFocus
goUp (_ , _    , []                         ) = Nothing
goUp (e , depth, (RightCrumb        e') : bc) = Just (e :$ e', depth-1, bc)
goUp (e', _    , (LeftCrumb   depth e ) : bc) = Just (e :$ e', depth  , bc)
goUp (e , _    , (LambdaCrumb depth v ) : bc) = Just (v :^ e , depth  , bc)

goUps :: Int -> ExprFocus -> Maybe ExprFocus
goUps 0 focus = Just focus
goUps n focus
  | n < 0     = Nothing
  | otherwise = goUps (n-1) =<< goUp focus

-- | フォーカスを root に移す
goRoot :: ExprFocus -> ExprFocus
goRoot focus = case goUp focus of
                    Nothing     -> focus
                    Just focus' -> goRoot focus'
