{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import ParserSpec
-- import ExprSpec
import FocusSpec
import PPrintSpec
import EvalSpec


main :: IO ()
main = hspec $ do
  specParserExpr
  specParserContext

  -- specExprIsFreeIn
  -- specExprResolve
  -- specExprUnlambda
  -- specExprSubst
  -- specExprCompile
  -- specExprApply
  -- specExprRename
  -- specExprRewrite

  specFocusGoLeftOrRightOrUpOrIntoLambda
  specFocusGoUps
  specFocusGoRoot

  specPPrintPp

  specEvalEval
