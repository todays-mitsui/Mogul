{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import TestParser
import TestExpr
import TestPPrint


main :: IO ()
main = hspec $ do
  testParserIdent
  testParserExpr
  testParserDef
  testParserContext
  testParserLineComment

  testExprIsFreeIn
  testExprResolve
  testExprUnlambda
  testExprSubst
  testExprCompile

  testPPrintPp
