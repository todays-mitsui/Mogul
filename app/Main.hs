{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Lazy   (liftIO, execStateT)

import System.Directory           (getCurrentDirectory)
import System.IO                  (IOMode (..), openFile, hSetEncoding, hFlush, stdout, utf8)
import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import Data.Functor               ((<$>))
import Data.List                  (splitAt)
import Control.Monad              (forever, void)
import Text.Parsec                (parse)
import Web.Scotty

import Data
import Eval

import Parser.Expr (parseExpr, parseContext)
import PPrint      (pp)

import CUI         (Mogul, initCUI, getCommand, runCommand)
import API         (api)


main :: IO ()
-- main = void $ execStateT main' emptyContext
main = scotty 8080 api

main' :: Mogul ()
main' = do
    initCUI
    forever $ do
        errOrCmd <- getCommand
        case errOrCmd of
            Left  parseError -> liftIO $ putStrLn . show $ parseError
            Right command    -> runCommand command

            -- main :: IO ()
-- main = do
--   cd      <- getCurrentDirectory
--   context <- loadContext $ cd ++ "/default.context"
--   putStrLn "Mogul v0.1.0"
--   putStrLn ""
--   putStrLn . pp $ context
--   forever $ do
--     putStr "> "
--     hFlush stdout
--     input <- T.getLine
--     case parseExpr input of
--          Left  parseError -> putStrLn . show $ parseError
--          Right e          ->
--            do putStrLn . pp $ e
--               let (es, cont) = splitAt 500 $ evals context e
--               mapM_ (putStrLn . ("⇒ " ++) . pp) es
--               if not (null cont)
--                  then putStrLn $ (show . length $ es) ++ " steps, and more..."
--                  else putStrLn $ (show . length $ es) ++ " steps, done."
--     putStrLn ""


loadContext :: String -> IO Context
loadContext filepath = do
  h <- openFile filepath ReadMode
  hSetEncoding h utf8
  eitherContext <- parseContext <$> T.hGetContents h
  case eitherContext of
       Left  parseError -> do putStrLn . show $ parseError
                              return emptyContext
       Right context    -> return context
