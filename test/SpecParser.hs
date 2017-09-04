{-# LANGUAGE OverloadedStrings #-}

module SpecParser (
  specParserIdent,
  specParserExpr,
  specParserDef,
  specParserContext,
  specParserLineComment
) where


import Test.Hspec
-- import Control.Exception (evaluate)

import Data.Function         (on)
import Control.Monad.Trans   (liftIO)
import Text.Parsec           (parse)
import Text.Parsec.Error     (ParseError, errorMessages)
import Data.Either           (isLeft)
import Data.ByteString.Char8 (singleton, intercalate)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Lazy as Map

import Data
import Parser hiding (context)
import qualified Parser as P


instance Eq ParseError where
  (==) =  (==) `on` errorMessages

--------------------------------------------------------------------------------

specParserIdent = describe "Parser.ident" $ do
  it "can parse single letter identifier, ex. 'x'" $ do
    parse ident "" "x" `shouldBe` Right (Ident "x")

  it "can parse multi letter identifier, ex. 'FOO_BAR'" $ do
    parse ident "" "FOO_BAR" `shouldBe` Right (Ident "FOO_BAR")

  it "can parse digit letter identifier, ex. '42'" $ do
    parse ident "" "42" `shouldBe` Right (Ident "42")


specParserExpr = describe "Parser.expr" $ do
  it "can parse apply statement, ex. '`xy'" $ do
    parse expr "" "`xy" `shouldBe` Right (Var x :$ Var y)

  it "can parse apply statement, ex. '`42 x'" $ do
    parse expr "" "`42x" `shouldBe` Right (Var (Ident "42") :$ Var x)

  it "can parse lambda abstraction statement, ex. '^x.x'" $ do
    parse expr "" "^x.x" `shouldBe` Right (x :^ Var x)

  it "can parse lambda abstraction statement, ex. '^42.42'" $ do
    parse expr "" "^42.42" `shouldBe` Right (Ident "42" :^ Var (Ident "42"))

  it "can parse multi variable lambda abstraction statement, '^xy.`yx'" $ do
    parse expr "" "^xy.`yx" `shouldBe` Right (x :^ y :^ Var y :$ Var x)

  context "when parse invalid expression" $ do
    it "return Left ParseError" $
      isLeft (parse expr "" "``xy")


specParserDef = describe "Parser.def" $ do
    it "can parse Ident define" $ do
      let src = intercalate (singleton '\n') ["``kxy = y", "```sxyz = ``xz`yz", "i = ``skk"]
      parse def "" src `shouldBe` Right (Ident "k", Func [x, y] (Var y))


specParserContext = describe "Parser.context" $ do
    it "can parse Ident defines" $ do
      let src = intercalate (singleton '\n') ["``kxy = y", "```sxyz = ``xz`yz", "i = ``skk"]
      parse P.context "" src
        `shouldBe` Right (
            Map.fromList [
              (Ident "i", Func [] (Var (Ident "s") :$ Var (Ident "k") :$ Var (Ident "k"))),
              (Ident "k", Func [x, y] (Var y)),
              (Ident "s", Func [x, y, z] (Var x :$ Var z :$ (Var y :$ Var z)))
            ]
          )


specParserLineComment = describe "Parser.lineComment" $ do
    it "can parse line comment, ex '# foo! bar!\\n'" $ do
      parse lineComment "" "# foo! bar!\n" `shouldBe` Right ()

--------------------------------------------------------------------------------

x = Ident "x"
y = Ident "y"
z = Ident "z"

n = Ident "n"
