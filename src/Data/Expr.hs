{-# LANGUAGE OverloadedStrings #-}

module Data.Expr
    ( Expr(..)
    ) where


import Data.Text  (Text)
import Data.Aeson

import Data.Ident (Ident(..))


-- | λ式
data Expr = Var !Ident      -- 変数
          | !Ident :^ Expr  -- 関数抽象
          | Expr   :$ Expr  -- 関数適用
          | Com !Ident      -- コンビネータ−
  deriving (Eq, Show, Read)

infixl 9 :$
infixr 7 :^

var :: Text -> Expr
var = Var . Ident

com :: Text -> Expr
com = Com . Ident

--------------------------------------------------------------------------------

i :: Expr
i = com "i"

k :: Expr
k = com "k"

s :: Expr
s = com "s"

--------------------------------------------------------------------------------

instance ToJSON Expr where
    toJSON (Var x) = object [
          "tag"        .= ("Variable" :: Text)
        , "identifier" .= x
        ]
    toJSON (x :^ e) = object [
          "tag"   .= ("Lambda" :: Text)
        , "param" .= x
        , "body"  .= e
        ]
    toJSON (el :$ er) = object [
          "tag"   .= ("Apply" :: Text)
        , "left"  .= el
        , "right" .= er
        ]
    toJSON (Com x) = object [
          "tag"        .= ("Combinator" :: Text)
        , "identifier" .= x
        ]
