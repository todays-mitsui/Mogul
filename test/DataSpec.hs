{-# LANGUAGE OverloadedStrings #-}

module DataSpec
    (
      specDataFunc
    ) where

import System.IO

import Test.Hspec
-- import Control.Exception (evaluate)

import Control.Monad.Trans (liftIO)

import Data


specDataFunc = describe "Data.Func" $ do
  let f = Func [x, y, z] (Var x :$ Var z :$ (Var y :$ Var z))

  it "can ask arity" $ do
    arity f `shouldBe` 3

  it "can ask Expr equivalent to a Func" $ do
    body f `shouldBe` (x :^ y :^ z :^ (Var x :$ Var z) :$ (Var y :$ Var z))

--------------------------------------------------------------------------------

x = Ident "x"
y = Ident "y"
z = Ident "z"
