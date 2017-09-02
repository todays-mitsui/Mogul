{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import SpecParser
import SpecExpr
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

  specPPrintPp
