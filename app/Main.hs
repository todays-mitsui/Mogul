{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Monad (forever)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Data
import Expr
import Eval

import Data.Functor ((<$>))
import Text.Parsec (parse)
import Parser      (context, expr, def)
import PPrint      (pp)
import Focus       (goRoot)

main :: IO ()
-- main = case parse def "" "```sxyz = ``xz`yz" of
--             Left  parseError -> putStrLn . show $ parseError
--             Right parsedExpr -> putStrLn . pp $ parsedExpr
main = do
  c <- loadContext $ Just "default.context"
  putStrLn . pp $ c
  forever $ do
    putStrLn "Input Lambda term:"
    putStr "> "
    input <- TIO.getLine
    case parse expr "" input of
      Left  parseError -> putStrLn . show $ parseError
      Right e          -> do putStrLn . pp $ e
                             mapM_ (putStrLn . pp) (reverse $ evals c e)
    putStrLn ""

  putStrLn ""
  let Right x = skk
  putStrLn . pp $ x
  mapM_ (putStrLn . pp) (reverse $ evals c x)

skk = parse expr "" "```skka"


loadContext :: Maybe String -> IO Context
loadContext Nothing         = return emptyContext
loadContext (Just filepath) = do
  eitherContext <- parse context "" <$> TIO.readFile filepath
  case eitherContext of
       Left  parseError -> do putStrLn . show $ parseError
                              return emptyContext
       Right context    -> return context
