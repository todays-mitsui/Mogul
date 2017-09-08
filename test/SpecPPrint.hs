{-# LANGUAGE OverloadedStrings #-}

module SpecPPrint (
  specPPrintPp
) where


import Test.Hspec
-- import Control.Exception (evaluate)

import Data
import PPrint


specPPrintPp = describe "PPrint.pp" $ do
    context "when pp(s :$ k :$ k)" $ do
      it "return '``skk'" $ do
        pp (s :$ k :$ k) `shouldBe` "``skk"

    context "when pp(FOO :$ BAR :$ BUZ)" $ do
      it "return '``FOO BAR BUZ'" $ do
        pp (Var (LargeIdent "FOO" Nothing) :$ Var (LargeIdent "BAR" Nothing) :$ Var (LargeIdent "BUZ" Nothing))
          `shouldBe` "``FOO BAR BUZ"

    context "when pp(s :$ _ :$ _)" $ do
      it "return '``s_ _'" $ do
        pp (s :$ Var (LargeIdent "_" Nothing) :$ Var (LargeIdent "_" Nothing))
          `shouldBe` "``s_ _"

    context "when pp(Func [y, x] (y :$ x))" $ do
      it "return '^x.^y.`yx'" $ do
        pp (Func [y, x] (Var y :$ Var x))
          `shouldBe` "^x.^y.`yx"

    context "when pp((s, Func [z, y, x] (x :$ z :$ (y :$ z))))" $ do
      it "return 's=^x.^y.^z.``xz`yz'" $ do
        pp (UniIdent "s", Func [z, y, x] $ Var x :$ Var z :$ (Var y :$ Var z))
          `shouldBe` "s=^x.^y.^z.``xz`yz"

--------------------------------------------------------------------------------

x = UniIdent "x"
y = UniIdent "y"
z = UniIdent "z"

n = UniIdent "n"

f = s :$ k :$ k
g = k :$ i
