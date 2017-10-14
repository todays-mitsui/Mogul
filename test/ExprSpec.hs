{-# LANGUAGE OverloadedStrings #-}

module ExprSpec
    (
      specExprAddIndex
    , specExprAddIndexF
    , specExprParseExpr
    , specExprParseContext
    ) where

import System.IO

import Test.Hspec
-- import Control.Exception (evaluate)

import Control.Monad.Trans (liftIO)
import Data.Function       (on)
import Data.Map.Lazy       (singleton)
import Text.Parsec         (ParseError)

import Data
import Expr


instance Eq ParseError where
    (==) = (==) `on` show

--------------------------------------------------------------------------------

specExprAddIndex = describe "Expr.addIndex" $ do
  it "retrun Indexed Expr" $
        -- ^f.^x.^y.``fyx
    let before = f :^ (x :^ (y :^ (Var Nothing  f :$ Var Nothing  y) :$ Var Nothing  x))
        after  = f :^ (x :^ (y :^ (Var (Just 2) f :$ Var (Just 0) y) :$ Var (Just 1) x))
    in  addIndex before `shouldBe` after

  it "retrun Indexed Expr" $
        -- `x^x.`x^x.`xx
    let before = Var Nothing x :$ (x :^ Var Nothing  x :$ (x :^ Var Nothing  x :$ Var Nothing  x))
        after  = Var Nothing x :$ (x :^ Var (Just 0) x :$ (x :^ Var (Just 0) x :$ Var (Just 0) x))
    in  addIndex before `shouldBe` after

specExprAddIndexF = describe "Expr.addIndexF" $ do
  it "retrun Indexed Func" $
        -- ```fxyz = ``xz`yz
    let before = Func [x, y, z] (Var Nothing  x :$ Var Nothing  z :$ (Var Nothing  y :$ Var Nothing  z))
        after  = Func [x, y, z] (Var (Just 2) x :$ Var (Just 0) z :$ (Var (Just 1) y :$ Var (Just 0) z))
    in  addIndexF before `shouldBe` after

  it "retrun Indexed Func" $
        -- ``fxy = ^g.``g`z`xx`z`xx
    let before = Func [x, y] (g :^ Var Nothing  g :$ (Var Nothing z :$ (Var Nothing  x :$ Var Nothing  x)) :$ (Var Nothing z :$ (Var Nothing  x :$ Var Nothing  x)))
        after  = Func [x, y] (g :^ Var (Just 0) g :$ (Var Nothing z :$ (Var (Just 2) x :$ Var (Just 2) x)) :$ (Var Nothing z :$ (Var (Just 2) x :$ Var (Just 2) x)))
    in  addIndexF before `shouldBe` after

specExprParseExpr = describe "Expr.parseExpr" $ do
  it "parse Indexed Expr" $
    let src = "^f.^x.^y.``fyx"
        e   = f :^ (x :^ (y :^ (Var (Just 2) f :$ Var (Just 0) y) :$ Var (Just 1) x))
    in  parseExpr src `shouldBe` Right e

  it "parse Indexed Expr" $
    let src = "`x^x.`x^x.`xx"
        e   = Var Nothing x :$ (x :^ Var (Just 0) x :$ (x :^ Var (Just 0) x :$ Var (Just 0) x))
    in  parseExpr src `shouldBe` Right e

specExprParseContext = describe "Expr.parseContext" $ do
  it "parse Context including Indexed Expr" $
    let src = "```FLIPfxy = ``fyx"
        e   = singleton (Ident "FLIP") $ Func [f, x, y] (Var (Just 2) f :$ Var (Just 0) y :$ Var (Just 1) x)
    in  parseContext src `shouldBe` Right e

--------------------------------------------------------------------------------

f = Ident "f"
g = Ident "g"
x = Ident "x"
y = Ident "y"
z = Ident "z"
