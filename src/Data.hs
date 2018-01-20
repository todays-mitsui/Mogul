{-# LANGUAGE OverloadedStrings #-}

module Data
    ( Ident(..)
    , isUniIdent
    , isLargeIdent

    , Expr(..)
    , var
    , com

    , Func(..)
    , arity, body

    , Context
    , emptyContext

    , s, k, i

    , Command(..)
    , Mogul

    , Nav(..)
    , ExtraExpr(..)
    , Transition(..)
    ) where


import Control.Monad.State.Lazy (StateT)
import Data.Text                  (Text)
import qualified Data.Text     as T
import Data.Char                  (isLower)
import Data.Set                   (Set)
import qualified Data.Set      as Set
import Data.Map.Lazy              (Map, (!))
import qualified Data.Map.Lazy as Map


-- | 識別子
-- |
-- | 有効な識別子は英小文字1文字から成るもの (ex. a,b,..,z)
-- | または、英大文字,アンダースコア(_),数字(0..9) の1文字以上の列
-- | ただし、有効な識別子のみ生成されるよう保証するのは Parser の役割とする
data Ident = Ident !Text
  deriving (Eq, Ord, Show, Read)

isUniIdent :: Ident -> Bool
isUniIdent (Ident x) = (T.length x == 1) && (isLower . T.head $ x)

isLargeIdent :: Ident -> Bool
isLargeIdent = not . isUniIdent

--------------------------------------------------------------------------------

-- | λ式
data Expr = Var !Ident      -- 変数
          | !Ident :^ Expr  -- 関数抽象
          | Expr   :$ Expr  -- 関数適用
          | Com !Ident      -- コンビネータ−
  deriving (Eq, Show, Read)

infixl 9 :$
infixr 7 :^

var :: Text -> Expr
var = Var . Ident

com :: Text -> Expr
com = Com . Ident

--------------------------------------------------------------------------------

-- | 無名関数
data Func = Func
    { params   :: [Ident]
    , bareExpr :: Expr
    } deriving (Eq, Show, Read)

arity :: Func -> Int
arity = length . params

body :: Func -> Expr
body (Func vs e) = foldr (:^) e vs

--------------------------------------------------------------------------------

-- | 関数定義の集合
type Context = Map Ident Func

emptyContext = Map.empty

--------------------------------------------------------------------------------

i :: Expr
i = com "i"

k :: Expr
k = com "k"

s :: Expr
s = com "s"

--------------------------------------------------------------------------------

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

type Mogul a = StateT Context IO a

--------------------------------------------------------------------------------

data Nav = NavLeft
         | NavRight
  deriving (Eq, Show)

data ExtraExpr = ExVar    !Ident
               | ExLambda !Ident    ExtraExpr
               | ExApply  ExtraExpr ExtraExpr Bool
               | ExCom    !Ident    (Maybe Func)
  deriving (Eq, Show)

--------------------------------------------------------------------------------

data Transition = Transition {
      _context    :: Context
    , _request    :: Expr
    , _transition :: [ExtraExpr]
    , _ellipsis   :: Int
    } deriving (Eq, Show)
