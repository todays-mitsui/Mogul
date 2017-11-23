{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Json
    ( jsonEncode
    , ExtraExpr(..)
    , addMetaInfo
    ) where


import Data.ByteString.Lazy.Internal (ByteString)
import Data.Text                  (Text)
import Data.Set                   (Set, member, empty, singleton, union)
import qualified Data.Set      as Set
import qualified Data.Map.Lazy as Map
-- import Control.Lens hiding (Context)
import Data.Aeson

import Data

jsonEncode :: Context -> [Nav] -> Expr -> ByteString
jsonEncode context navs e = let e' = addMetaInfo context (Just navs) e
                            in  encode e'

--------------------------------------------------------------------------------

data ExtraExpr = ExVar    !Ident    (Maybe Int)
               | ExLambda !Ident    ExtraExpr
               | ExApply  ExtraExpr ExtraExpr Bool
               | ExCom    !Ident    (Maybe Func)
  deriving (Eq, Show)

--------------------------------------------------------------------------------

instance ToJSON ExtraExpr where
    toJSON (ExVar x i) = object [
          "tag"        .= ("Variable" :: Text)
        , "identifier" .= x
        , "serialnum"  .= i
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

instance ToJSON Func where
    toJSON (Func xs e) = object [
          "tag"    .= ("Function" :: Text)
        , "params" .= xs
        , "body"   .= e
        ]

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

instance ToJSON Ident where
    toJSON (Ident x) = toJSON x

--------------------------------------------------------------------------------

addMetaInfo :: Context -> Maybe [Nav] -> Expr -> ExtraExpr
addMetaInfo context _ (Var x)
    | x `Map.member` context = ExVar x (Just 0)
    | otherwise              = ExVar x Nothing

addMetaInfo context _ (Com x) = ExCom x $ x `Map.lookup` context

addMetaInfo context navs@(Just (NavLeft:_)) (el :$ er) =
    let el' = addMetaInfo context (fmap tail navs) el
        er' = addMetaInfo context Nothing          er
    in  ExApply el' er' False

addMetaInfo context navs@(Just (NavRight:_)) (el :$ er) =
    let el' = addMetaInfo context Nothing          el
        er' = addMetaInfo context (fmap tail navs) er
    in  ExApply el' er' False

addMetaInfo context navs@(Just []) (el :$ er) =
    let el' = addMetaInfo context navs  el
        er' = addMetaInfo context Nothing er
    in  ExApply el' er' True

addMetaInfo context navs@Nothing (el :$ er) =
    let el' = addMetaInfo context Nothing el
        er' = addMetaInfo context Nothing er
    in  ExApply el' er' False

addMetaInfo context _ (x :^ e) = ExLambda x $ addMetaInfo context Nothing e
