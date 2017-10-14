{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import ParserSpec
import ExprSpec
import UnlambdaSpec
-- import FocusSpec
import PPrintSpec


main :: IO ()
main = hspec $ do
    specParserIdent
    specParserExpr
    specParserDef
    specParserContext
    specParserLineComment

    specExprAddIndex
    specExprAddIndexF
    specExprParseExpr
    specExprParseContext

    specUnlambdaExists
    -- specExprIsFreeIn
    -- specExprResolve
    specUnlambdaUnlambda
    -- specExprSubst
    -- specExprCompile
    -- specExprApply
    -- specExprRename
    -- specExprRewrite

    -- specFocusGoLeftOrRightOrUpOrIntoLambda
    -- specFocusGoUps
    -- specFocusGoRoot

    specPPrintPp
