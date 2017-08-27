{-# LANGUAGE OverloadedStrings #-}

module Main where


import Text.Parsec (parse)
import Parser      (expr, def)
import PPrint      (pp)

main :: IO ()
main = case parse def "" "```sxyz = ``xz`yz" of
            Left  parseError -> putStrLn . show $ parseError
            Right parsedExpr -> putStrLn . pp $ parsedExpr

skk = parse expr "" "``skk"
