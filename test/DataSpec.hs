{-# LANGUAGE OverloadedStrings #-}

module DataSpec
    (
      specDataEq
    , specDataFunc
    ) where

import System.IO

import Test.Hspec
-- import Control.Exception (evaluate)

import Control.Monad.Trans (liftIO)

import Data


specDataEq = describe "Data.eq" $ do
  it "compare Exprs" $
    Var Nothing x == Var Nothing x
  it "compare Exprs" $
    Var Nothing x /= Var Nothing y
  it "compare Exprs" $
    x :^ Var (Just 0) x == y :^ Var (Just 0) y
  it "compare Exprs" $
    x :^ Var Nothing y /= y :^ Var (Just 0) y

specDataFunc = describe "Data.Func" $ do
  let f = Func [x, y, z] (Var (Just 2) x :$ Var (Just 0) z :$ (Var (Just 1) y :$ Var (Just 0) z))

  it "can ask arity" $ do
    arity f `shouldBe` 3

  it "can ask Expr equivalent to a Func" $ do
    body f `shouldBe` (x :^ y :^ z :^ (Var (Just 2) x :$ Var (Just 0) z) :$ (Var (Just 1) y :$ Var (Just 0) z))

--------------------------------------------------------------------------------

x = Ident "x"
y = Ident "y"
z = Ident "z"
