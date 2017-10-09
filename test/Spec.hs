{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import ParserSpec
import ExprSpec
-- import FocusSpec
import PPrintSpec


main :: IO ()
main = hspec $ do
    specParserIdent
    specParserExpr
    specParserDef
    specParserContext
    specParserLineComment

    specExprExists
    -- specExprIsFreeIn
    -- specExprResolve
    specExprUnlambda
    -- specExprSubst
    -- specExprCompile
    -- specExprApply
    -- specExprRename
    -- specExprRewrite

    -- specFocusGoLeftOrRightOrUpOrIntoLambda
    -- specFocusGoUps
    -- specFocusGoRoot

    specPPrintPp
