{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import ParserSpec
import ExprSpec
import FocusSpec
import PPrintSpec


main :: IO ()
main = hspec $ do
  specParserIdent
  specParserExpr
  specParserDef
  specParserContext
  specParserLineComment

  specExprIsFreeIn
  specExprResolve
  specExprUnlambda
  specExprSubst
  specExprCompile
  specExprApply
  specExprRename

  specFocusGoLeftOrRightOrUpOrIntoLambda
  specFocusGoUps
  specFocusGoRoot

  specPPrintPp
