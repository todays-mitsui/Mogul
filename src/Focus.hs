module Focus where


import Data.Functor ((<$>))

import Data (Expr(..), Ident)


type ArgCount = Int

data ExprCrumb = RightCrumb           Expr
               | LeftCrumb   ArgCount Expr
               | LambdaCrumb ArgCount Ident
               deriving (Eq, Show, Read)

type ExprFocus = (Expr, ArgCount, [ExprCrumb])

--------------------------------------------------------------------------------

-- | 関数適用の左側(関数側)にフォーカスを移す
goLeft :: ExprFocus -> Maybe ExprFocus
goLeft ((e :$ e'), argCount, bc) = Just (e, argCount+1, RightCrumb e' : bc)
goLeft _                         = Nothing

-- | 関数適用の右側(引数側)にフォーカスを移す
goRight :: ExprFocus -> Maybe ExprFocus
goRight ((e :$ e'), argCount, bc) = Just (e', 0, LeftCrumb argCount e : bc)
goRight _                         = Nothing

--------------------------------------------------------------------------------

-- | 関数抽象の内側にフォーカスを移す
goIntoLambda :: ExprFocus -> Maybe ExprFocus
goIntoLambda ((v :^ e), argCount, bc) = Just (e, 0, LambdaCrumb argCount v : bc)
goIntoLambda _                        = Nothing

--------------------------------------------------------------------------------

-- | フォーカスを一つ外側に移す
goUp :: ExprFocus -> Maybe ExprFocus
goUp (_ , _       , []                            ) = Nothing
goUp (e , argCount, (RightCrumb           e') : bc) = Just (e :$ e', argCount-1, bc)
goUp (e', _       , (LeftCrumb   argCount e ) : bc) = Just (e :$ e', argCount  , bc)
goUp (e , _       , (LambdaCrumb argCount v ) : bc) = Just (v :^ e , argCount  , bc)

goUps :: Int -> ExprFocus -> Maybe ExprFocus
goUps 0 focus = Just focus
goUps n focus
  | n < 0     = Nothing
  | otherwise = goUp focus >>= goUps (n-1)

-- | フォーカスを root に移す
goRoot :: ExprFocus -> ExprFocus
goRoot focus = case goUp focus of
                    Nothing     -> focus
                    Just focus' -> goRoot focus'

rootExpr :: ExprFocus -> Expr
rootExpr focus = let (e, _, _) = goRoot focus
                 in  e


-- rollupArgs :: Int -> ExprFocus -> Maybe ((Expr, [Expr]), ArgCount, [ExprCrumb])
-- rollupArgs = undefined

-- rollupRight