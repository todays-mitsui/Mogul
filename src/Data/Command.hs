module Data.Command
    ( Command(..)
    ) where


import Data.Ident     (Ident)
import Data.Expr      (Expr)
import Data.Func      (Func)
import Data.Context   (Context)
import Data.ExtraExpr (ExtraExpr)


data Command = CmdEvals    Expr
             | CmdEvalLast Expr
             | CmdEvalHead !Int   Expr
             | CmdEvalTail !Int   Expr
             | CmdInfo     !Ident
             | CmdStore    !Ident Func
             | CmdDelete   !Ident
             | CmdShowContext
             -- | CmdLoadContext FilePath
             -- | CmdSaveContext FilePath
             -- | CmdUnlambda Expr
             -- | CmdHelp
             | CmdNull
             | CmdQuit
  deriving (Eq, Show)
