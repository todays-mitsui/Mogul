{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (
  specParserExpr,
  specParserContext,
) where


import Test.Hspec
-- import Control.Exception (evaluate)

import Data.Function         (on)
import Control.Monad.Trans   (liftIO)
import Text.Parsec           (parse)
import Text.Parsec.Error     (ParseError, errorMessages)
import Data.Either           (isLeft)
import Data.Text             (singleton, intercalate)
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map

import Data
import Parser.Expr hiding (context, ident)
import qualified Parser.Expr as P


instance Eq ParseError where
  (==) =  (==) `on` errorMessages

--------------------------------------------------------------------------------

specParserExpr = describe "Parser.expr" $ do
  it "can parse apply statement, ex. '`xy'" $ do
    parseExpr "`xy" `shouldBe` Right (Com x :$ Com y)

  it "can parse apply statement, ex. '`42 x'" $ do
    parseExpr "`42x" `shouldBe` Right (Com ident42 :$ Com x)

  it "can parse lambda abstraction statement, ex. '^x.x'" $ do
    parseExpr "^x.x" `shouldBe` Right (x :^ Var x)

  it "can parse lambda abstraction statement, ex. '^42.42'" $ do
    parseExpr "^42.42" `shouldBe` Right (ident42 :^ Var ident42)

  it "can parse multi variable lambda abstraction statement, '^xy.`yx'" $ do
    parseExpr "^xy.`yx" `shouldBe` Right (x :^ y :^ Var y :$ Var x)

  context "when parse invalid expression" $ do
    it "return Left ParseError" $
      isLeft (parseExpr "``xy")

specParserContext = describe "Parser.context" $ do
    it "can parse Ident defines" $ do
      let src = intercalate (singleton '\n') ["``kxy = y", "```sxyz = ``xz`yz", "i = ``skk"]
      parseContext src
        `shouldBe` Right (
            Map.fromList [
              (Ident "i", Func [] (Com (Ident "s") :$ Com (Ident "k") :$ Com (Ident "k"))),
              (Ident "k", Func [x, y] (Var y)),
              (Ident "s", Func [x, y, z] (Var x :$ Var z :$ (Var y :$ Var z)))
            ]
          )

--------------------------------------------------------------------------------

x = Ident "x"
y = Ident "y"
z = Ident "z"

n = Ident "n"

ident42 = Ident "42"
