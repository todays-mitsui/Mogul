{-# LANGUAGE OverloadedStrings #-}

module UnlambdaSpec (
  specUnlambdaIsFreeIn,
  specUnlambdaResolve,
  specUnlambdaUnlambda,
  specUnlambdaSubst,
  specUnlambdaCompile,
  specUnlambdaApply,
  specUnlambdaRename,
  specUnlambdaRewrite
) where

import System.IO

import Test.Hspec
-- import Control.Exception (evaluate)

import Control.Monad.Trans   (liftIO)
import Text.Parsec           (parse)
import Text.Parsec.Error     (ParseError, errorMessages)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Set (fromList)

import Data
import Parser hiding (context)
import qualified Parser as P
import Unlambda


readSampleContext :: IO (Either ParseError Context)
readSampleContext = parse P.context "" <$> do
  h <- openFile "test/sample.context" ReadMode
  hSetEncoding h utf8
  T.hGetContents h

--------------------------------------------------------------------------------

specUnlambdaIsFreeIn = describe "x `Unlambda.isFreeIn` expr" $ do
  context "when x is free in expr" $ do
    it "retrun True" $
      x `isFreeIn` (y :^ Var y :$ Var x)

  context "when x is bound in expr" $ do
    it "retrun False" $
      not (x `isFreeIn` (x :^ Var x :$ Var y))


specUnlambdaResolve = describe "Unlambda.resolve" $ do
  it "resolve ^x.x to i" $ do
    resolve x (Var x) `shouldBe` i

  it "resolve ^x.y to `ky" $ do
    resolve x (Var y) `shouldBe` (k :$ Var y)

  it "resolve ^x.``fgx to `fg" $ do
    resolve x (f :$ g :$ Var x) `shouldBe` (f :$ g)

  it "resolve ^x.``fx`gx to ``s^x.`fx^x.`gx" $ do
    resolve x (f :$ Var x :$ (g :$ Var x))
      `shouldBe` (s :$ resolve x (f :$ Var x) :$ resolve x (g :$ Var x))


specUnlambdaUnlambda = describe "Unlambda.unlambda" $ do
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


specUnlambdaSubst = describe "Unlambda.subst" $ do
  it "substitute ISZERO combination of subfunctions" $ do
    Right context <- liftIO readSampleContext
    subst context (Var (LargeIdent "ISZERO" Nothing))
      `shouldBe` (n :^ (Var n :$ (LargeIdent "_" Nothing :^ x :^ y :^ Var y)) :$ (x :^ y :^ Var x))

  it "dosent substitute undefined variable" $ do
    Right context <- liftIO readSampleContext
    subst context (Var (LargeIdent "FOO" Nothing))
      `shouldBe` Var (LargeIdent "FOO" Nothing)

  it "dosent substitute undefined variable" $ do
    Right context <- liftIO readSampleContext
    subst context (Var (LargeIdent "NAMED" Nothing))
      `shouldBe` (LargeIdent "NAMED"  Nothing :^ Var (LargeIdent "NAMED" Nothing) :$ Var (LargeIdent "NAMED" Nothing))


specUnlambdaCompile = describe "Unlambda.compile" $ do
  it "compile ISZERO to SKI Combinator" $ do
    Right context <- liftIO readSampleContext
    compile context (Var (LargeIdent "ISZERO" Nothing))
      `shouldBe` (s :$ (s :$ i :$ (k :$ (k :$ (k :$ i)))) :$ (k :$ k))

  it "dosent compile undefined variable" $ do
    Right context <- liftIO readSampleContext
    compile context (Var (LargeIdent "FOO" Nothing))
      `shouldBe` Var (LargeIdent "FOO" Nothing)

  it "..." $ do
    Right context <- liftIO readSampleContext
    compile context (Var (LargeIdent "NAMED" Nothing))
      `shouldBe` (s :$ i :$ i)

specUnlambdaApply = describe "Unlambda.apply" $ do
  -- it "can call Func with given Args" $ do
  --   Right context <- liftIO readSampleContext
  --   apply context (LargeIdent "FLIP" Nothing) [Var (LargeIdent "F" Nothing), Var (LargeIdent "X" Nothing), Var (LargeIdent "Y" Nothing)]
  --     `shouldBe` (Var (LargeIdent "F" Nothing) :$ Var (LargeIdent "Y" Nothing) :$ Var (LargeIdent "X" Nothing))

  it "can call Undefined Variable also" $ do
    Right context <- liftIO readSampleContext
    apply context (LargeIdent "UNDEFINED_FUNC" Nothing) [Var x, Var y, Var z]
      `shouldBe` (Var (LargeIdent "UNDEFINED_FUNC" Nothing) :$ Var x :$ Var y :$ Var z)

specUnlambdaRename = describe "Unlambda.rename" $ do
  it "rename reserved Variable" $
    let reserved = fromList [x, y, z]
    in  rename reserved x /= x

  it "keep non-reserved Variable" $
    let reserved = fromList [x, y, z]
    in  rename reserved n == n

specUnlambdaRewrite = describe "Unlambda.rewrite" $ do
  it "..." $ do
    let y' = LargeIdent "Y" $ Just 0
    rewrite x (Var x :$ Var y) (y :^ Var y)
      `shouldBe` ((y' :^ Var y') :$ Var y)


--------------------------------------------------------------------------------

f = Ident "f"
g = Ident "g"
x = Ident "x"
y = Ident "y"
z = Ident "z"
