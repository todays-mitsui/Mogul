{-# LANGUAGE OverloadedStrings #-}

module TestPPrint (
  testPPrintPp
) where


import Test.Hspec
-- import Control.Exception (evaluate)

import Control.Monad.Trans   (liftIO)

import Data
import PPrint


testPPrintPp = describe "PPrint.pp" $ do
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
