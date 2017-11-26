{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module API where


import Data.Monoid
import Data.Text.Lazy (pack)
import Data.Text.Internal.Lazy (Text)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty
import Network.Wai.Middleware.Static
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics


api = do
    -- middleware $ staticPolicy $ cssPolicy <|> jsPolicy <|> imgPolicy

    get "/" $ do
        file "public/index.html"

    matchAny "/eval" $ do
        req <- jsonData `rescue` (\msg -> return $ Request "foo" msg)
        -- addHeader "Access-Control-Allow-Origin" "*"
        json req

    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

    middleware $ staticPolicy $ addBase "static" <> extsPolicy

    notFound (text "404: Not found!")


data Request = Request {
      request :: Text
    , expr    :: Text
    } deriving (Eq, Show, Generic)

instance ToJSON Request
instance FromJSON Request

allowExts = ["html", "css", "js", "png", "jpg", "gif"]

extsPolicy = mconcat $ map mkPolicy allowExts
  where
    mkPolicy ext = hasSuffix $ "." <> ext
