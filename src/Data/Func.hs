{-# LANGUAGE OverloadedStrings #-}

module Data.Func
    ( Func(..)
    , arity
    , body
    ) where


import Data.Text  (Text)
import Data.Aeson

import Data.Ident (Ident)
import Data.Expr  (Expr(..))


-- | 無名関数
data Func = Func
    { params   :: [Ident]
    , bareExpr :: Expr
    } deriving (Eq, Show, Read)

arity :: Func -> Int
arity = length . params

body :: Func -> Expr
body (Func vs e) = foldr (:^) e vs


instance ToJSON Func where
    toJSON (Func xs e) = object [
          "tag"    .= ("Function" :: Text)
        , "params" .= xs
        , "body"   .= e
        ]
