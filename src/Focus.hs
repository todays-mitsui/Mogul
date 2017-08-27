module Focus where


import Expr (Expr(..), Ident, s, k, i)


data ExprCrumb = RightCrumb Expr
               | LeftCrumb Expr
               | LambdaCrumb Ident
               deriving (Eq, Show, Read)

type ExprFocus = (Expr, [ExprCrumb])

--------------------------------------------------------------------------------

-- | 関数適用の左側(関数側)にフォーカスを移す
goLeft :: ExprFocus -> Maybe ExprFocus
goLeft ((e :$ e'), bc) = Just (e, RightCrumb e' : bc)
goLeft _               = Nothing

-- | 関数適用の右側(引数側)にフォーカスを移す
goRight :: ExprFocus -> Maybe ExprFocus
goRight ((e :$ e'), bc) = Just (e', LeftCrumb e : bc)
goRight _               = Nothing

--------------------------------------------------------------------------------

-- | 関数抽象の内側にフォーカスを移す
goIntoLambda :: ExprFocus -> Maybe ExprFocus
goIntoLambda ((v :^ e), bc) = Just (e, LambdaCrumb v : bc)
goIntoLambda _              = Nothing

--------------------------------------------------------------------------------

-- | フォーカスを一つ外側に移す
goUp :: ExprFocus -> Maybe ExprFocus
goUp (_ , [])                   = Nothing
goUp (e , (RightCrumb e') : bc) = Just (e :$ e', bc)
goUp (e', (LeftCrumb  e ) : bc) = Just (e :$ e', bc)
goUp (e , (LambdaCrumb v) : bc) = Just (v :^ e , bc)

-- | フォーカスを root に移す
goRoot :: ExprFocus -> ExprFocus
goRoot focus = case goUp focus of
                    Nothing     -> focus
                    Just focus' -> goRoot focus'
