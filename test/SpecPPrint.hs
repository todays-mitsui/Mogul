{-# LANGUAGE OverloadedStrings #-}

module SpecPPrint (
  specPPrintPp
) where


import Test.Hspec
-- import Control.Exception (evaluate)

import Control.Monad.Trans   (liftIO)

import Data
import PPrint


specPPrintPp = describe "PPrint.pp" $ do
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

    context "when pp((s, Func [z, y, x] (x :$ z :$ (y :$ z))))" $ do
      it "return 's=^x.^y.^z.``xz`yz'" $ do
        pp (Ident "s", Func [z, y, x] $ Var x :$ Var z :$ (Var y :$ Var z))
          `shouldBe` "s=^x.^y.^z.``xz`yz"

--------------------------------------------------------------------------------

x = Ident "x"
y = Ident "y"
z = Ident "z"

n = Ident "n"

f = s :$ k :$ k
g = k :$ i
