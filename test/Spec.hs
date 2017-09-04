{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import SpecParser
import SpecExpr
import SpecFocus
import SpecPPrint


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

  specFocusGoLeftOrRightOrUpOrIntoLambda
  specFocusGoUps
  specFocusGoRoot

  specPPrintPp
