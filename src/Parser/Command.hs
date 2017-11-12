{-# LANGUAGE OverloadedStrings #-}

module Parser.Command
    ( parseCommand
    ) where


import Control.Monad (void)
import Control.Applicative hiding ((<|>), (*>), (<*))
import Data.Map.Lazy (insert, delete)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Text.Parsec hiding (token)
import Text.Parsec.Text (Parser)

import Data
import Parser.Expr (subst, substF, expr, ident, def)


parseCommand :: Text -> Either ParseError Command
parseCommand src = let result = parse cmd "stdin" src
                   in  s <$> result
  where
    s (CmdEvals e)      = CmdEvals $ subst e
    s (CmdEvalLast e)   = CmdEvalLast $ subst e
    s (CmdEvalHead n e) = CmdEvalHead n $ subst e
    s (CmdEvalTail n e) = CmdEvalTail n $ subst e
    s (CmdStore x f)    = CmdStore x $ substF f
    s command           = command

--------------------------------------------------------------------------------

cmd :: Parser Command
cmd = try cmdEvalLast
      <|> try cmdInfo
      <|> try cmdDelete
      <|> try cmdShowContext
      <|> try cmdQuit
      <|> try cmdStore
      <|> try cmdEvalHead
      <|> try cmdEvalTail
      <|> try cmdEvals
      <|> cmdNull

--------------------------------------------------------------------------------

cmdEvals :: Parser Command
cmdEvals = do
    e <- token expr
    eof
    return $ CmdEvals e

--------------------------------------------------------------------------------

optLast :: Parser ()
optLast = void $ try (string ":last") <|> string ":l" <|> string "!"

cmdEvalLast :: Parser Command
cmdEvalLast = do
    spaces
    optLast
    many1 space
    e <- token expr
    eof
    return $ CmdEvalLast e

--------------------------------------------------------------------------------

optHead1 :: Parser Int
optHead1 = do
    char ':'
    ds <- many1 digit
    return $ read ds

optHead2 :: Parser Int
optHead2 = do
    try (string ":head") <|> string ":h"
    spaces
    ds <- many1 digit
    return $ read ds

cmdEvalHead :: Parser Command
cmdEvalHead = do
    spaces
    n <- try optHead1 <|> optHead2
    e <- token expr
    eof
    return $ CmdEvalHead n e

--------------------------------------------------------------------------------

optTail1 :: Parser Int
optTail1 = do
    string ":-"
    ds <- many1 digit
    return $ read ds

optTail2 :: Parser Int
optTail2 = do
    try (string ":tail") <|> string ":t"
    spaces
    ds <- many1 digit
    return $ read ds

cmdEvalTail :: Parser Command
cmdEvalTail = do
    spaces
    n <- try optTail1 <|> optTail2
    e <- token expr
    eof
    return $ CmdEvalTail n e

--------------------------------------------------------------------------------

optInfo :: Parser ()
optInfo = void $ try (string ":info") <|> string ":i" <|> string "?"

cmdInfo :: Parser Command
cmdInfo = do
    spaces
    optInfo
    many1 space
    x <- token ident
    eof
    return $ CmdInfo x

--------------------------------------------------------------------------------

cmdStore :: Parser Command
cmdStore = do
    (x, func) <- token def
    eof
    return $ CmdStore x func

--------------------------------------------------------------------------------

optDelete :: Parser ()
optDelete = void $ try (string ":delete") <|> string ":d"

cmdDelete :: Parser Command
cmdDelete = do
    spaces
    optDelete
    many1 space
    x <- token ident
    eof
    return $ CmdDelete x

--------------------------------------------------------------------------------

optContext :: Parser ()
optContext = void $ try (string ":context") <|> string ":c"

cmdShowContext :: Parser Command
cmdShowContext = do
    token optContext
    eof
    return CmdShowContext

--------------------------------------------------------------------------------

cmdNull :: Parser Command
cmdNull = do
    spaces
    eof
    return CmdNull

--------------------------------------------------------------------------------

optQuit :: Parser ()
optQuit = void $ try (string ":q") <|> string ":quit"

cmdQuit :: Parser Command
cmdQuit = do
    token optQuit
    eof
    return CmdQuit

--------------------------------------------------------------------------------

-- | パーサーの前の空白, 空行, 行コメントを読み飛ばすような新しいパーサーを返す
token :: Parser a -> Parser a
token p = spaces *> p <* spaces
