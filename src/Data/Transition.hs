{-# LANGUAGE OverloadedStrings #-}

module Data.Transition
    ( Transition
    , mkTransition
    ) where


import Data.Maybe                 (listToMaybe)
import Data.Map.Lazy              (mapKeysMonotonic)
import Data.Aeson

import Data.Ident     (Ident(..))
import Data.Expr      (Expr)
import Data.Context   (Context)
import Data.ExtraExpr (ExtraExpr)


data Transition = Transition {
      _context    :: Context
    , _request    :: Expr
    , _transition :: [ExtraExpr]
    , _ellipsis   :: Int
    } deriving (Eq, Show)


mkTransition :: Context -> Expr -> [ExtraExpr] -> Int -> Transition
mkTransition = Transition

--------------------------------------------------------------------------------

instance ToJSON Transition where
    toJSON = do
        context    <- _context
        request    <- _request
        len        <- _ellipsis
        (es, cont) <- splitAt len . _transition
        return $ object [
              "context"    .= mapKeysMonotonic unIdent context
            , "request"    .= request
            , "transition" .= es
            , "next"       .= listToMaybe cont
            ]
      where
        unIdent (Ident x) = x
