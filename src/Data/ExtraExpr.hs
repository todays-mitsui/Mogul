{-# LANGUAGE OverloadedStrings #-}

module Data.ExtraExpr
    ( ExtraExpr(..)
    ) where


import Data.Text  (Text)
import Data.Aeson

import Data.Ident (Ident)
import Data.Func  (Func)


data ExtraExpr = ExVar    !Ident
               | ExLambda !Ident    ExtraExpr
               | ExApply  ExtraExpr ExtraExpr Bool
               | ExCom    !Ident    (Maybe Func)
  deriving (Eq, Show)

--------------------------------------------------------------------------------

instance ToJSON ExtraExpr where
    toJSON (ExVar x) = object [
          "tag"        .= ("Variable" :: Text)
        , "identifier" .= x
        ]
    toJSON (ExLambda x e) = object [
          "tag"   .= ("Lambda" :: Text)
        , "param" .= x
        , "body"  .= toJSON e
        ]
    toJSON (ExApply el er b) = object [
          "tag"             .= ("Apply" :: Text)
        , "isHeadBetaRedex" .= b
        , "left"            .= el
        , "right"           .= er
        ]
    toJSON (ExCom x f) = object [
          "tag"        .= ("Combinator" :: Text)
        , "identifier" .= x
        , "function"   .= f
        ]
