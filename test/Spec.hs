{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import DataSpec
import ParserSpec
-- import ExprSpec
-- import UnlambdaSpec
-- import FocusSpec
import PPrintSpec
import EvalSpec


main :: IO ()
main = hspec $ do
  specDataFunc

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

  -- specFocusGoLeftOrRightOrUpOrIntoLambda
  -- specFocusGoUps
  -- specFocusGoRoot

  specPPrintPp

  specEvalEval
