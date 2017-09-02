{-# LANGUAGE OverloadedStrings #-}

module SpecFocus (
  specFocusGoLeftOrRightOrUpOrIntoLambda,
  specFocusGoUps,
  specFocusGoRoot
) where


import Test.Hspec
-- import Control.Exception (evaluate)

import Data
import Focus


specFocusGoLeftOrRightOrUpOrIntoLambda = describe "Focus.go(Left|Right|Up|IntoLambda)" $ do
  context "when in (``skk, 0, [])" $ do
    let focus = (s :$ k :$ k, 0 , [])

    it "goLeft return Just (`sk, 1, [RightCrumb k])" $ do
      goLeft focus
        `shouldBe` Just (s :$ k, 1, [RightCrumb k])

    it "goRight return Just (k, 0, [LeftCrumb 0 `sk])" $ do
      goRight focus
        `shouldBe` Just (k, 0, [LeftCrumb 0 (s :$ k)])

    it "goUp return Nothing" $ do
      goUp focus
        `shouldBe` Nothing

    it "goIntoLambda return Nothing" $ do
      goIntoLambda focus
        `shouldBe` Nothing

  context "when in (`sk, 1, [RightCrumb k])" $ do
    let focus = (s :$ k, 1, [RightCrumb k])

    it "goLeft return Just (s, 2, [RightCrumb k, RightCrumb k])" $ do
      goLeft focus
        `shouldBe` Just (s, 2, [RightCrumb k, RightCrumb k])

    it "goRight return Just (k, 0, [LeftCrumb 1 s, RightCrumb k])" $ do
      goRight focus
        `shouldBe` Just (k, 0, [LeftCrumb 1 s, RightCrumb k])

    it "goUp return Just (``skk, 0, [])" $ do
      goUp focus
        `shouldBe` Just (s :$ k :$ k, 0, [])

    it "goIntoLambda return Nothing" $ do
      goIntoLambda focus
        `shouldBe` Nothing

  context "when in (k, 0, [LeftCrumb 0 `sk])" $ do
    let focus = (k, 0, [LeftCrumb 0 (s :$ k)])

    it "goLeft return Nothing" $ do
      goLeft focus
        `shouldBe` Nothing

    it "goRight return Nothing" $ do
      goRight focus
        `shouldBe` Nothing

    it "goUp return Just (``skk, 0, [])" $ do
      goUp focus
        `shouldBe` Just (s :$ k :$ k, 0, [])

    it "goIntoLambda return Nothing" $ do
      goIntoLambda focus
        `shouldBe` Nothing

  context "when in (k, 0, [LeftCrumb 1 s, RightCrumb k])" $ do
    let focus = (k, 0, [LeftCrumb 1 s, RightCrumb k])

    it "goLeft return Nothing" $ do
      goLeft focus
        `shouldBe` Nothing

    it "goRight return Nothing" $ do
      goRight focus
        `shouldBe` Nothing

    it "goUp return Just (`sk, 1, [RightCrumb k])" $ do
      goUp focus
        `shouldBe` Just (s :$ k, 1, [RightCrumb k])

    it "goIntoLambda return Nothing" $ do
      goIntoLambda focus
        `shouldBe` Nothing

  context "when in (^y.`yx`, 0, [LambdaCrumb 0 x])" $ do
    let focus = (y :^ Var y :$ Var x, 0, [LambdaCrumb 0 x])

    it "goLeft return Nothing" $ do
      goLeft focus
        `shouldBe` Nothing

    it "goRight return Nothing" $ do
      goRight focus
        `shouldBe` Nothing

    it "goUp return Just (^x.^y.`yx, 0, [])" $ do
      goUp focus
        `shouldBe` Just (x :^ y :^ Var y :$ Var x, 0, [])

    it "goIntoLambda return Just (`yx, 0, [LambdaCrumb 0 y, LambdaCrumb 0 x])" $ do
      goIntoLambda focus
        `shouldBe` Just (Var y :$ Var x, 0, [LambdaCrumb 0 y, LambdaCrumb 0 x])

specFocusGoUps = describe "Focus.goUps" $ do
  context "when in (k, 0, [LeftCrumb 1 s, RightCrumb k])" $ do
    let focus = (k, 0, [LeftCrumb 1 s, RightCrumb k])

    it "goUps 0 return Just (k, 0, [LeftCrumb 1 s, RightCrumb k])" $ do
      goUps 0 focus
        `shouldBe` Just (k, 0, [LeftCrumb 1 s, RightCrumb k])

    it "goUps 1 return Just (`sk, 0, [RightCrumb k])" $ do
      goUps 1 focus
        `shouldBe` Just (s :$ k, 1, [RightCrumb k])

    it "goUps 2 return Just (``skk, 0, [])" $ do
      goUps 2 focus
        `shouldBe` Just (s :$ k :$ k, 0, [])

    it "goUps 3 and more return Nothing" $ do
      goUps 3 focus `shouldBe` Nothing
      goUps 4 focus `shouldBe` Nothing
      goUps 5 focus `shouldBe` Nothing

specFocusGoRoot = describe "Focus.goRoot" $ do
  context "when in (s, 2, [RightCrumb k, RightCrumb k])" $ do
    it "goRoot return (``skk, 0, [])" $ do
      goRoot (s, 2, [RightCrumb k, RightCrumb k])
        `shouldBe` (s :$ k :$ k, 0, [])

  context "when in (k, 0, [LeftCrumb 1 s, RightCrumb k])" $ do
    it "goRoot return (``skk, 0, [])" $ do
      goRoot (k, 0, [LeftCrumb 1 s, RightCrumb k])
        `shouldBe` (s :$ k :$ k, 0, [])

  context "when in (`sk, 1, [RightCrumb k])" $ do
    it "goRoot return (``skk, 0, [])" $ do
      goRoot (s :$ k, 1, [RightCrumb k])
        `shouldBe` (s :$ k :$ k, 0, [])

  context "when in (k, 0, [LeftCrumb 0 `sk])" $ do
    it "goRoot return (``skk, 0, [])" $ do
      goRoot (k, 0, [LeftCrumb 0 (s :$ k)])
        `shouldBe` (s :$ k :$ k, 0, [])

  context "when in (``skk, 0, [])" $ do
    it "goRoot return (``skk, 0, [])" $ do
      goRoot (s :$ k :$ k, 0, [])
        `shouldBe` (s :$ k :$ k, 0, [])

--------------------------------------------------------------------------------

x = Ident "x"
y = Ident "y"
z = Ident "z"

n = Ident "n"

f = s :$ k :$ k
g = k :$ i
