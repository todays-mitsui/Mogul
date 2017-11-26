{-# LANGUAGE OverloadedStrings #-}

module JsonSpec
  ( specJsonAddMetaInfo
  , specJsonJsonEncode
  ) where


import Test.Hspec
-- import Control.Exception (evaluate)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T

import Data
import Parser.Expr
import Json (jsonEncode, ExtraExpr(..), addMetaInfo)


Right c = parseContext . T.unlines $ [
    "`ix = x"
  , "``kxy = x"
  , "```sxyz = ``xz`yz"
  , "`Yf = `^x.`f`xx^x.`f`xx"
  ]

specJsonAddMetaInfo = describe "Json.addMetaInfo" $ do
    context "`i```skkx" $
      it "return ExtraExpr" $ do
        let Right e = parseExpr "`i```skkx"
        addMetaInfo c (Just []) e `shouldBe` ExApply (ExCom (Ident "i") (Just (Func {params = [Ident "x"], bareExpr = Var (Ident "x")}))) (ExApply (ExApply (ExApply (ExCom (Ident "s") (Just (Func {params = [Ident "x",Ident "y",Ident "z"], bareExpr = (Var (Ident "x") :$ Var (Ident "z")) :$ (Var (Ident "y") :$ Var (Ident "z"))}))) (ExCom (Ident "k") (Just (Func {params = [Ident "x",Ident "y"], bareExpr = Var (Ident "x")}))) False) (ExCom (Ident "k") (Just (Func {params = [Ident "x",Ident "y"], bareExpr = Var (Ident "x")}))) False) (ExCom (Ident "x") Nothing) False) True

    context "`k```skkx" $
      it "return ExtraExpr" $ do
        let Right e = parseExpr "`k```skkx"
        addMetaInfo c (Just [NavRight]) e `shouldBe` ExApply (ExCom (Ident "k") (Just (Func {params = [Ident "x",Ident "y"], bareExpr = Var (Ident "x")}))) (ExApply (ExApply (ExApply (ExCom (Ident "s") (Just (Func {params = [Ident "x",Ident "y",Ident "z"], bareExpr = (Var (Ident "x") :$ Var (Ident "z")) :$ (Var (Ident "y") :$ Var (Ident "z"))}))) (ExCom (Ident "k") (Just (Func {params = [Ident "x",Ident "y"], bareExpr = Var (Ident "x")}))) True) (ExCom (Ident "k") (Just (Func {params = [Ident "x",Ident "y"], bareExpr = Var (Ident "x")}))) True) (ExCom (Ident "x") Nothing) True) False

    context "`ix" $
      it "return ExtraExpr" $ do
        let Right e = parseExpr "`ix"
        addMetaInfo c (Just []) e `shouldBe` ExApply (ExCom (Ident "i") (Just (Func {params = [Ident "x"], bareExpr = Var (Ident "x")}))) (ExCom (Ident "x") Nothing) True

    context "`^x.x y" $
      it "return ExtraExpr" $ do
        let Right e = parseExpr "`^x.x y"
        addMetaInfo c (Just []) e `shouldBe` ExApply (ExLambda (Ident "x") (ExVar (Ident "x") Nothing)) (ExCom (Ident "y") Nothing) True

    context "`^s.s y" $
      it "return ExtraExpr" $ do
        let Right e = parseExpr "`^s.s y"
        addMetaInfo c (Just []) e `shouldBe` ExApply (ExLambda (Ident "s") (ExVar (Ident "s") $ Just 0)) (ExCom (Ident "y") Nothing) True

specJsonJsonEncode = describe "Json.encode" $ do
    context "```skkx" $
      it "return json string" $ do
        let Right e = parseExpr "```skkx"
        BS.putStrLn $ jsonEncode c [] e
        jsonEncode c [] e `shouldBe` "{\"tag\":\"Apply\",\"left\":{\"tag\":\"Apply\",\"left\":{\"tag\":\"Apply\",\"left\":{\"function\":{\"tag\":\"Function\",\"body\":{\"tag\":\"Apply\",\"left\":{\"tag\":\"Apply\",\"left\":{\"tag\":\"Variable\",\"identifier\":\"x\"},\"right\":{\"tag\":\"Variable\",\"identifier\":\"z\"}},\"right\":{\"tag\":\"Apply\",\"left\":{\"tag\":\"Variable\",\"identifier\":\"y\"},\"right\":{\"tag\":\"Variable\",\"identifier\":\"z\"}}},\"params\":[\"x\",\"y\",\"z\"]},\"tag\":\"Combinator\",\"identifier\":\"s\"},\"isHeadBetaRedex\":true,\"right\":{\"function\":{\"tag\":\"Function\",\"body\":{\"tag\":\"Variable\",\"identifier\":\"x\"},\"params\":[\"x\",\"y\"]},\"tag\":\"Combinator\",\"identifier\":\"k\"}},\"isHeadBetaRedex\":true,\"right\":{\"function\":{\"tag\":\"Function\",\"body\":{\"tag\":\"Variable\",\"identifier\":\"x\"},\"params\":[\"x\",\"y\"]},\"tag\":\"Combinator\",\"identifier\":\"k\"}},\"isHeadBetaRedex\":true,\"right\":{\"function\":null,\"tag\":\"Combinator\",\"identifier\":\"x\"}}"
