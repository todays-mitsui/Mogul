{-# LANGUAGE OverloadedStrings #-}

module CUI
    ( Command(..)
    , Mogul
    , initCUI
    , getCommand
    , runCommand
    ) where


import Control.Monad.State.Lazy
import System.IO (IOMode (..), openFile, hSetEncoding, hFlush, stdout, utf8)
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess)
import Data.Map.Lazy (insert, delete)
import qualified Data.Map.Lazy as Map
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Text.Parsec (ParseError)

import Data
import Parser.Expr    (parseContext)
import Parser.Command (parseCommand)
import PPrint (pp)
import Eval (evals)


initCUI :: Mogul ()
initCUI = do
    cd      <- liftIO $ getCurrentDirectory
    context <- liftIO $ loadContext $ cd ++ "/default.mgl"
    put context
    liftIO $ putStrLn "Mogul v0.1.0.0"
    liftIO $ putStrLn ""

loadContext :: String -> IO Context
loadContext filepath = do
    h <- openFile filepath ReadMode
    hSetEncoding h utf8
    eitherContext <- parseContext <$> T.hGetContents h
    case eitherContext of
         Left  parseError -> do putStrLn . show $ parseError
                                return emptyContext
         Right context    -> return context

--------------------------------------------------------------------------------

runCommand :: Command -> Mogul ()
runCommand (CmdEvals e)      = runEval e
runCommand (CmdEvalLast e)   = runEvalLast e
runCommand (CmdEvalHead n e) = runEvalHead n e
runCommand (CmdEvalTail n e) = runEvalTail n e
runCommand (CmdInfo x)       = runInfo x
runCommand (CmdStore x f)    = runStore x f
runCommand (CmdDelete x)     = runDelete x
runCommand CmdShowContext    = runShowContext
runCommand CmdNull           = runNull
runCommand CmdQuit           = runQuit


runEval :: Expr -> Mogul ()
runEval = runEvalHead 1000

runEvalLast :: Expr -> Mogul ()
runEvalLast = runEvalTail 1

runEvalHead :: Int -> Expr -> Mogul ()
runEvalHead n e = do
    context <- get
    let (es, cont) = splitAt n $ evals context e
    liftIO $ putStrLn . pp $ e
    liftIO $ mapM_ (putStrLn . ("⇒ " ++) . pp) es
    if not (null cont)
       then liftIO $ do putStrLn "⇒ ..."
                        putStrLn $ (show . length $ es) ++ " steps, and more..."
       else liftIO $ putStrLn $ (show . length $ es) ++ " steps, done."
    liftIO $ putStrLn ""

runEvalTail :: Int -> Expr -> Mogul ()
runEvalTail n e = do
    context <- get
    let (es, cont) = splitAt 10000 $ evals context e
    let len = length es
    liftIO $ putStrLn . pp $ e
    if len >= n
       then liftIO $ putStrLn "⇒ ..."
       else return ()
    liftIO $ mapM_ (putStrLn . ("⇒ " ++) . pp) $ drop (len - n) es
    if not (null cont)
       then liftIO $ putStrLn $ (show len) ++ " steps, and more..."
       else liftIO $ putStrLn $ (show len) ++ " steps, done."
    liftIO $ putStrLn ""

runInfo :: Ident -> Mogul ()
runInfo x = do
    context <- get
    case x `Map.lookup` context of
         Just f  -> liftIO $ putStrLn . pp $ (x, f)
         Nothing -> liftIO $ putStrLn "undefined"
    liftIO $ putStrLn ""

runStore :: Ident -> Func -> Mogul ()
runStore x f = modify $ insert x f

runDelete :: Ident -> Mogul ()
runDelete x = modify $ delete x

runShowContext :: Mogul ()
runShowContext = do
    context <- get
    liftIO $ putStrLn . pp $ context
    liftIO $ putStrLn ""


runNull :: Mogul ()
runNull = return ()

runQuit :: Mogul ()
runQuit = liftIO $ void exitSuccess

--------------------------------------------------------------------------------

getCommand :: Mogul (Either ParseError Command)
getCommand = liftIO $ do
    putStr "> "
    hFlush stdout
    input <- T.getLine
    return $ parseCommand input
