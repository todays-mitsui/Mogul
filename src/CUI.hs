{-# LANGUAGE OverloadedStrings #-}

module CUI (
      Command(..)
    , Mogul
    , initCUI
    , getCommand
    , runCommand ) where


import System.IO (IOMode (..), openFile, hSetEncoding, hFlush, stdout, utf8)
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess)
import Control.Monad (void)
import Control.Monad.State.Lazy
import Control.Applicative hiding ((<|>), (*>), (<*))
import Data.Map.Lazy (insert, delete)
import qualified Data.Map.Lazy as Map
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.Text (Text, pack, singleton)
import Text.Parsec hiding (token)
import Text.Parsec.Text

import Data
import Parser (parseContext, subst, substF, expr, ident, def)
import PPrint (pp)
import Eval (evals)


data Command = CmdEvals    Expr
             | CmdEvalLast Expr
             | CmdInfo     Ident
             | CmdStore    Ident Func
             | CmdDelete   Ident
             | CmdShowContext
             -- | CmdLoadContext FilePath
             -- | CmdSaveContext FilePath
             -- | CmdUnlambda Expr
             -- | CmdHelp
             | CmdNull
             | CmdQuit
  deriving (Eq, Show)

type Mogul a = StateT Context IO a


--------------------------------------------------------------------------------

initCUI :: Mogul ()
initCUI = do
    cd      <- liftIO $ getCurrentDirectory
    context <- liftIO $ loadContext $ cd ++ "/default.context"
    put context
    liftIO $ putStrLn "Mogul v0.0.1.0"
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
runCommand (CmdEvals e)    = runEval e
runCommand (CmdEvalLast e) = runEvalLast e
runCommand (CmdInfo x)     = runInfo x
runCommand (CmdStore x f)  = runStore x f
runCommand (CmdDelete x)   = runDelete x
runCommand CmdShowContext  = runShowContext
runCommand CmdNull         = runNull
runCommand CmdQuit         = runQuit


runEval :: Expr -> Mogul ()
runEval e = do
    context <- get
    let (es, cont) = splitAt 1000 $ evals context e
    liftIO $ putStrLn . pp $ e
    liftIO $ mapM_ (putStrLn . ("⇒ " ++) . pp) es
    if not (null cont)
       then liftIO $ putStrLn $ (show . length $ es) ++ " steps, and more..."
       else liftIO $ putStrLn $ (show . length $ es) ++ " steps, done."
    liftIO $ putStrLn ""

runEvalLast :: Expr -> Mogul ()
runEvalLast e = do
    context <- get
    let (es, cont) = splitAt 10000 $ evals context e
    liftIO $ putStrLn . pp $ e
    liftIO $ putStrLn . ("⇒⇒ " ++) . pp . last $ es
    if not (null cont)
        then liftIO $ putStrLn $ (show . length $ es) ++ " steps, and more..."
        else liftIO $ putStrLn $ (show . length $ es) ++ " steps, done."
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

--------------------------------------------------------------------------------

parseCommand :: Text -> Either ParseError Command
parseCommand src = let result = parse cmd "stdin" src
                   in  s <$> result
  where
    s (CmdEvals e)    = CmdEvals $ subst e
    s (CmdEvalLast e) = CmdEvalLast $ subst e
    s (CmdStore x f)  = CmdStore x $ substF f
    s command         = command

cmd :: Parser Command
cmd = try cmdEvalLast
      <|> try cmdInfo
      <|> try cmdDelete
      <|> try cmdShowContext
      <|> try cmdQuit
      <|> try cmdStore
      <|> try cmdEvals
      <|> cmdNull

cmdEvals :: Parser Command
cmdEvals = do
    e <- token expr
    eof
    return $ CmdEvals e

cmdEvalLast :: Parser Command
cmdEvalLast = do
    spaces
    try (string ":last") <|> string ":l" <|> string "!"
    many1 space
    e <- token expr
    eof
    return $ CmdEvalLast e

cmdInfo :: Parser Command
cmdInfo = do
    spaces
    try (string ":info") <|> string ":i" <|> string "?"
    many1 space
    x <- token ident
    eof
    return $ CmdInfo x

cmdStore :: Parser Command
cmdStore = do
    (x, func) <- token def
    eof
    return $ CmdStore x func

cmdDelete :: Parser Command
cmdDelete = do
    spaces
    try (string ":delete") <|> string ":d"
    many1 space
    x <- token ident
    eof
    return $ CmdDelete x

cmdShowContext :: Parser Command
cmdShowContext = do
    token $ try (string ":context") <|> string ":c"
    eof
    return CmdShowContext

cmdNull :: Parser Command
cmdNull = do
    spaces
    eof
    return CmdNull

cmdQuit :: Parser Command
cmdQuit = do
    token $ try (string ":q") <|> string ":quit"
    eof
    return CmdQuit

-- | パーサーの前の空白, 空行, 行コメントを読み飛ばすような新しいパーサーを返す
token :: Parser a -> Parser a
token p = spaces *> p <* spaces
