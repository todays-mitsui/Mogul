module Data.Transiton (
      Transition
    ) where


data Transition = Transition {
      _context    :: Context
    , _request    :: Expr
    , _transition :: [ExtraExpr]
    , _ellipsis   :: Int
    } deriving (Eq, Show)


mkTransition :: Context -> Expr -> [ExtraExpr] -> Int -> Transition
mkTransition = Transition
