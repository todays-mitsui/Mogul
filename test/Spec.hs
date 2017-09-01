{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
-- import Test.QuickCheck
import Control.Exception (evaluate)

import Data.Function         (on)
import Control.Monad.Trans   (liftIO)
import Text.Parsec
import Text.Parsec.Error     (errorMessages)
import Data.Either           (isLeft)
import Data.ByteString.Char8 (singleton, intercalate)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Lazy as Map

import Data
import Expr
import Parser hiding (context)
import qualified Parser as P
import PPrint


instance Eq ParseError where
  (==) =  (==) `on` errorMessages


main :: IO ()
main = hspec $ do
  describe "Parser.ident" $ do
    it "can parse single letter identifier, ex. 'x'" $ do
      parse ident "" "x" `shouldBe` Right (Ident "x")

    it "can parse multi letter identifier, ex. 'FOO_BAR'" $ do
      parse ident "" "FOO_BAR" `shouldBe` Right (Ident "FOO_BAR")

    it "can parse digit letter identifier, ex. '42'" $ do
      parse ident "" "42" `shouldBe` Right (Ident "42")

  describe "Parser.expr" $ do
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

  describe "Parser.def" $ do
    it "can parse Ident define" $ do
      let src = intercalate (singleton '\n') ["``kxy = y", "```sxyz = ``xz`yz", "i = ``skk"]
      parse def "" src `shouldBe` Right (Ident "k", Func 2 (x :^ y :^ Var y))

  describe "Parser.context" $ do
    it "can parse Ident defines" $ do
      let src = intercalate (singleton '\n') ["``kxy = y", "```sxyz = ``xz`yz", "i = ``skk"]
      parse P.context "" src
        `shouldBe` Right (
            Map.fromList [
              (Ident "i", Func 0 (Var (Ident "s") :$ Var (Ident "k") :$ Var (Ident "k"))),
              (Ident "k", Func 2 (x :^ y :^ Var y)),
              (Ident "s", Func 3 (x :^ y :^ z :^ Var x :$ Var z :$ (Var y :$ Var z)))
            ]
          )

  describe "Parser.lineComment" $ do
    it "can parse line comment, ex '# foo! bar!\\n'" $ do
      parse lineComment "" "# foo! bar!\n" `shouldBe` Right ()

  describe "x `Expr.isFreeIn` expr" $ do
    context "when x is free in expr" $ do
      it "retrun True" $
        x `isFreeIn` (y :^ Var y :$ Var x)

    context "when x is bound in expr" $ do
      it "retrun False" $
        not (x `isFreeIn` (x :^ Var x :$ Var y))

  describe "Expr.resolve" $ do
    it "resolve ^x.x to i" $ do
      resolve x (Var x) `shouldBe` i

    it "resolve ^x.y to `ky" $ do
      resolve x (Var y) `shouldBe` (k :$ Var y)

    it "resolve ^x.``fgx to `fg" $ do
      resolve x (f :$ g :$ Var x) `shouldBe` (f :$ g)

    it "resolve ^x.``fx`gx to ``s^x.`fx^x.`gx" $ do
      resolve x (f :$ Var x :$ (g :$ Var x))
        `shouldBe` (s :$ resolve x (f :$ Var x) :$ resolve x (g :$ Var x))

  describe "Expr.unlambda" $ do
    it "resolve ^x.x to i" $ do
      unlambda (x :^ Var x) `shouldBe` i

    it "resolve ^x.y to `ky" $ do
      unlambda (x :^ Var y) `shouldBe` (k :$ Var y)

    it "resolve ^xy.`yx to ``s`k`sik" $ do
      unlambda (x :^ y :^ Var y :$ Var x)
        `shouldBe` (s :$ (k :$ (s :$ i)) :$ k)

    it "resolve ^x.`xx to ``sii" $ do
      unlambda (x :^ Var x :$ Var x) `shouldBe` (s :$ i :$ i)

    -- The Y combinator, discovered by Haskell B. Curry
    it "resolve ^x.`^y.`x`yy^y.`x`yy to ``s``s``s`ksk`k`sii``s``s`ksk`k``sii" $ do
      unlambda (x :^ (y :^ Var x :$ (Var y :$ Var y)) :$ (y :^ Var x :$ (Var y :$ Var y)))
        `shouldBe` (s :$ (s :$ (s :$ (k :$ s) :$ k) :$ (k :$ (s :$ i :$ i))) :$ (s :$ (s :$ (k :$ s) :$ k) :$ (k :$ (s :$ i :$ i))))

  describe "Expr.subst" $ do
    it "substitute ISZERO combination of subfunctions" $ do
      Right context <- liftIO $ parse P.context "" <$> BS.readFile "test/sample.context"
      subst context (Var (Ident "ISZERO"))
        `shouldBe` (n :^ (Var n :$ (Ident "_" :^ x :^ y :^ Var y)) :$ (x :^ y :^ Var x))

    it "dosent substitute undefined variable" $ do
      Right context <- liftIO $ parse P.context "" <$> BS.readFile "test/sample.context"
      subst context (Var (Ident "FOO"))
        `shouldBe` Var (Ident "FOO")

    it "dosent substitute undefined variable" $ do
      Right context <- liftIO $ parse P.context "" <$> BS.readFile "test/sample.context"
      subst context (Var (Ident "NAMED"))
        `shouldBe` (Ident "NAMED" :^ Var (Ident "NAMED") :$ Var (Ident "NAMED"))

  describe "Expr.compile" $ do
    it "compile ISZERO to SKI Combinator" $ do
      Right context <- liftIO $ parse P.context "" <$> BS.readFile "test/sample.context"
      compile context (Var (Ident "ISZERO"))
        `shouldBe` (s :$ (s :$ i :$ (k :$ (k :$ (k :$ i)))) :$ (k :$ k))

    it "dosent compile undefined variable" $ do
      Right context <- liftIO $ parse P.context "" <$> BS.readFile "test/sample.context"
      compile context (Var (Ident "FOO"))
        `shouldBe` Var (Ident "FOO")

    it "dosent compile undefined variable" $ do
      Right context <- liftIO $ parse P.context "" <$> BS.readFile "test/sample.context"
      compile context (Var (Ident "NAMED"))
        `shouldBe` (s :$ i :$ i)

  describe "PPrint.pp" $ do
    context "when pp(s :$ k :$ k)" $ do
      it "return '``skk'" $ do
        pp (s :$ k :$ k) `shouldBe` "``skk"

    context "when pp(FOO :$ BAR :$ BUZ)" $ do
      it "return '``FOO BAR BUZ'" $ do
        pp (Var (Ident "FOO") :$ Var (Ident "BAR") :$ Var (Ident "BUZ"))
          `shouldBe` "``FOO BAR BUZ"

    context "when pp(s :$ _ :$ _)" $ do
      it "return '``s_ _'" $ do
        pp (s :$ Var (Ident "_") :$ Var (Ident "_"))
          `shouldBe` "``s_ _"

--------------------------------------------------------------------------------

x = Ident "x"
y = Ident "y"
z = Ident "z"

n = Ident "n"

f = s :$ k :$ k
g = k :$ i
