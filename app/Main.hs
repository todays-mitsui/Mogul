{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.ByteString.Char8 as BS

import Expr
import Eval

import Text.Parsec (parse)
import Parser      (context, expr, def)
import PPrint      (pp)

main :: IO ()
-- main = case parse def "" "```sxyz = ``xz`yz" of
--             Left  parseError -> putStrLn . show $ parseError
--             Right parsedExpr -> putStrLn . pp $ parsedExpr
main = do
  c <- loadContext $ Just "default.context"
  putStrLn . pp $ c
  putStrLn ""
  let Right x = skk
  putStrLn . pp $ x
  putStrLn . show $ eval c (x, 0 , [])

skk = parse expr "" "```skkw"


loadContext :: Maybe String -> IO Context
loadContext Nothing         = return emptyContext
loadContext (Just filepath) = do
  eitherContext <- parse context "" <$> BS.readFile filepath
  case eitherContext of
       Left  parseError -> do putStrLn . show $ parseError
                              return emptyContext
       Right context    -> return context
