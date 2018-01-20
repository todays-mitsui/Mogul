{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Json where


import Data.ByteString.Lazy       (ByteString)
import Data.Text                  (Text)
import Data.Set                   (Set, member, empty, singleton, union)
import qualified Data.Set      as Set
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy              (mapKeysMonotonic)
import Data.Maybe                 (listToMaybe)
-- import Control.Lens hiding (Context)
import Data.Aeson

import Data

-- jsonEncode :: Context -> [Nav] -> Expr -> ByteString
-- jsonEncode context navs e = let e' = addMetaInfo context (Just navs) e
--                             in  encode e'

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

