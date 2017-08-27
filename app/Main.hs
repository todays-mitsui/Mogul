{-# LANGUAGE OverloadedStrings #-}

module Main where


import Text.Parsec (parse)
import Parser      (expr)

main :: IO ()
main = case parse expr "" "^xyz.``xz`yz" of
            Left  parseError -> putStrLn . show $ parseError
            Right parsedExpr -> putStrLn . show $ parsedExpr
