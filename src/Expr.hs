{-# LANGUAGE OverloadedStrings #-}

module Expr (
    addIndex
  , addIndexF
  , parseExpr
  , parseContext
) where


import Control.Applicative          ((<$>))
import qualified Data.Map.Strict as Map
import Data.Map.Strict              (Map, (!), insert)
import Data.Text                    (Text)

import Text.Parsec (ParseError, parse)

import Data
import Parser (expr, context)


type IndexDict = Map Ident Index

addIndex :: Expr -> Expr
addIndex = addIndex' Map.empty

addIndex' :: IndexDict -> Expr ->Expr
addIndex' indexes (Var _ x)
    | x `Map.notMember` indexes = Var Nothing x
    | otherwise                 = let i = indexes ! x
                                  in  Var (Just i) x

addIndex' indexes (e :$ e')     = addIndex' indexes e :$ addIndex' indexes e'

addIndex' indexes (v :^ e)      = let newIndexes = insert v 0 $ update indexes
                                  in  v :^ addIndex' newIndexes e

update :: IndexDict -> IndexDict
update = fmap (+1)

--------------------------------------------------------------------------------

addIndexF :: Func -> Func
addIndexF (Func args bareExpr) = Func args (addIndex' indexes bareExpr)
  where
    count   = length args
    pairs   = zip args $ reverse [0 .. count -1]
    indexes = Map.fromList pairs

--------------------------------------------------------------------------------

parseExpr :: Text -> Either ParseError Expr
parseExpr src = addIndex <$> parse expr "" src

parseContext :: Text -> Either ParseError Context
parseContext src = parse context "" src <$$> \context ->
                       addIndexF <$> context
  where
    xs <$$> f = f <$> xs
