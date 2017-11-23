{-# LANGUAGE OverloadedStrings #-}

module EvalSpec
  ( specEvalEval
  , specEvalEvalsPlus
  ) where


import Test.Hspec
-- import Control.Exception (evaluate)

import qualified Data.Text     as T

import Data
import Parser.Expr (parseExpr, parseContext)
import Eval (eval, evals, evalPlus, evalsPlus)


Right c = parseContext . T.unlines $ [
    "`ix = x"
  , "``kxy = x"
  , "```sxyz = ``xz`yz"
  , "`Yf = `^x.`f`xx^x.`f`xx"
  ]

specEvalEval = describe "Eval.eval" $ do
    context "Expr has β-redex" $
      it "return list of Expr reduced only one β-redex" $ do
        let Right e1 = parseExpr "```skkx"
        eval c e1 `shouldBe` [com "k" :$ com "x" :$ (com "k" :$ com "x")]

        let Right e2 = parseExpr "```s`ix`iy`iz"
        eval c e2 `shouldBe` [
            com "i" :$ com "x" :$ (com "i" :$ com "z") :$ (com "i" :$ com "y" :$ (com "i" :$ com "z"))
          , com "s" :$ com "x" :$ (com "i" :$ com "y") :$ (com "i" :$ com "z")
          , com "s" :$ (com "i" :$ com "x") :$ com "y" :$ (com "i" :$ com "z")
          , com "s" :$ (com "i" :$ com "x") :$ (com "i" :$ com "y") :$ com "z"
          ]

        let Right e3 = parseExpr "```ix`iz``iy`iz"
        eval c e3 `shouldBe` [
            com "x" :$ (com "i" :$ com "z") :$ (com "i" :$ com "y" :$ (com "i" :$ com "z"))
          , com "i" :$ com "x" :$ com "z" :$ (com "i" :$ com "y" :$ (com "i" :$ com "z"))
          , com "i" :$ com "x" :$ (com "i" :$ com "z") :$ (com "y" :$ (com "i" :$ com "z"))
          , com "i" :$ com "x" :$ (com "i" :$ com "z") :$ (com "i" :$ com "y" :$ com "z")
          ]

        let Right e4 = parseExpr "`^x.^y.``xyy``kyx"
        eval c e4 `shouldBe` [
            Ident "y" :^ (com "k" :$ com "y" :$ com "x") :$ var "y" :$ var "y"
          , (Ident "x" :^ Ident "y" :^ var "x" :$ var "y" :$ var "y") :$ com "y"
          ]

    context "Expr has no β-redex" $
      it "return empty list" $ do
        let Right e1 = parseExpr "x"
        eval c e1 `shouldBe` []

        let Right e2 = parseExpr "```xy`yz`zx"
        eval c e2 `shouldBe` []

specEvalEvalsPlus = describe "Eval.evalsPlus" $ do
    context "Expr has β-redex" $
      it "return" $ do
        let Right e1 = parseExpr "```s`ik`ik`ix"
        evalsPlus c e1 `shouldBe` [
            (com "s" :$ (com "i" :$ com "k") :$ (com "i" :$ com "k") :$ (com "i" :$ com "x")           , []                )
          , (com "i" :$ com "k" :$ (com "i" :$ com "x") :$ (com "i" :$ com "k" :$ (com "i" :$ com "x")), [NavLeft, NavLeft])
          , (com "k" :$ (com "i" :$ com "x") :$ (com "i" :$ com "k" :$ (com "i" :$ com "x"))           , []                )
          , (com "i" :$ com "x"                                                                        , []                )
          , (com "x"                                                                                   , []                )
          ]
