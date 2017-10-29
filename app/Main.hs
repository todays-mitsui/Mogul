{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO                 (IOMode (..), openFile, hSetEncoding, utf8)
import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import Data.Functor ((<$>))
import Control.Monad             (forever)
import Text.Parsec (parse)

import Data
-- import Expr
import Eval

import Parser      (parseExpr, parseContext)
import PPrint      (pp)
-- import Focus       (goRoot)


main :: IO ()
-- main = case parse def "" "```sxyz = ``xz`yz" of
--             Left  parseError -> putStrLn . show $ parseError
--             Right parsedExpr -> putStrLn . pp $ parsedExpr
main = do
  c <- loadContext "default.context"
  putStrLn . pp $ c
  putStrLn $ show skk
  -- mapM_ (putStrLn . show) . eval c [] $ skk
  mapM_ (putStrLn . pp . uncrumb) . eval c [] $ skk
  -- forever $ do
  --   putStrLn "Input Lambda term:"
  --   putStr "> "
  --   input <- TIO.getLine
  --   case parse expr "" input of
  --     Left  parseError -> putStrLn . show $ parseError
  --     Right e          -> do putStrLn . pp $ e
  --                            mapM_ (putStrLn . pp) (reverse $ evals c e)
  --   putStrLn ""

  -- putStrLn ""
  -- let Right x = skk
  -- putStrLn . pp $ x
  -- mapM_ (putStrLn . pp) (reverse $ evals c x)

Right skk = parseExpr "``ik``^x.^y.`yxab"

loadContext :: String -> IO Context
loadContext filepath = do
  h <- openFile filepath ReadMode
  hSetEncoding h utf8
  eitherContext <- parseContext <$> T.hGetContents h
  case eitherContext of
       Left  parseError -> do putStrLn . show $ parseError
                              return emptyContext
       Right context    -> return context
