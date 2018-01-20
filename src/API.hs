{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module API where


import Data.Monoid
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty
import Network.Wai.Middleware.Static
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics

import System.IO (IOMode (..), openFile, hSetEncoding, hFlush, stdout, utf8)

import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import Data.Text (Text)
-- import Data.Map.Lazy (lookup)
import System.Directory (getCurrentDirectory)
import Data (Context, emptyContext)
import Parser.Expr (parseExpr, parseContext)
import Eval (evals, transition)
import Json


api = do
    -- middleware $ staticPolicy $ cssPolicy <|> jsPolicy <|> imgPolicy

    get "/" $ do
        file "webui/public/index.html"

    matchAny "/eval" $ do
        req <- jsonData `rescue` (\_ -> return $ Request "foo" "bar")
        -- addHeader "Access-Control-Allow-Origin" "*"

        cd      <- liftIO $ getCurrentDirectory
        context <- liftIO $ loadContext $ cd ++ "/default.context"

        let Right e = parseExpr $ exprStr req

        json $ transition context e

    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

    middleware $ staticPolicy $ addBase "static" <> extsPolicy

    notFound (text "404: Not found!")


data Request = Request {
      request :: Text
    , exprStr :: Text
    } deriving (Eq, Show, Generic)

instance ToJSON Request
instance FromJSON Request

allowExts = ["html", "css", "js", "png", "jpg", "gif"]

extsPolicy = mconcat $ map mkPolicy allowExts
  where
    mkPolicy ext = hasSuffix $ "." <> ext

--------------------------------------------------------------------------------

-- mkExtraExpr :: Context -> Expr -> ExtraExpr
-- mkExtraExpr context (Var x)    = ExVar x
-- mkExtraExpr context (x  :^ e)  = ExLambda x $ mkExtraExpr context e
-- mkExtraExpr context (el :$ er) = ExApply (mkExtraExpr context el) (mkExtraExpr context er)
-- mkExtraExpr context (Com x)    = ExCom x $ context `lookup` x


--------------------------------------------------------------------------------

loadContext :: String -> IO Context
loadContext filepath = do
  h <- openFile filepath ReadMode
  hSetEncoding h utf8
  eitherContext <- parseContext <$> T.hGetContents h
  case eitherContext of
       Left  parseError -> do putStrLn . show $ parseError
                              return emptyContext
       Right context    -> return context
