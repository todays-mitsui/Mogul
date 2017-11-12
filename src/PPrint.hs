{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module PPrint
    ( pp
    ) where


import Data.Monoid            ((<>))
import qualified Data.Text as T
import Data.Text              (Text, pack, unpack)
import Data.Map.Lazy          (foldrWithKey)

import Data


class PPrintable a where
    prepara :: a -> [Token]
    pp      :: a -> String
    pp = render . prepara


render :: [Token] -> String
render []       = ""
render (s@(LargeSymbol _) : ps@(LargeSymbol _ : _))
                = show s <> " " <> render ps
render (s@(Symbol _) : ps@(LargeSymbol _ : _))
                = show s <> " " <> render ps
render (p : ps) = show p <> render ps

--------------------------------------------------------------------------------

instance PPrintable Ident where
    prepara = (:[]) . symbol

instance PPrintable Expr where
    prepara = flatten []

instance PPrintable Func where
    prepara = prepara . body

instance PPrintable (Ident, Func) where
    prepara (v, f) = symbol v : Equal : prepara f

instance PPrintable Context where
    prepara = foldrWithKey (\v f done -> prepara (v, f) <> (EOL : done)) []

--------------------------------------------------------------------------------

data Token = Backquote         -- ^ "`"
           | Lambda            -- ^ "^"
           | Dot               -- ^ "."
           | Equal             -- ^ "="
           | Symbol Text       -- ^ 英小文字1文字からなるシンボル
           | LargeSymbol Text  -- ^ 英数字2文字以上からなるシンボル
           | EOL               -- ^ 行端
  deriving (Eq)

instance Show Token where
    show Backquote       = "`"
    show Lambda          = "^"
    show Dot             = "."
    show Equal           = "="
    show (Symbol s)      = unpack s
    show (LargeSymbol s) = unpack s
    show EOL             = "\n"

--------------------------------------------------------------------------------

flatten :: [Token] -> Expr -> [Token]
flatten acc (e :$ e') = Backquote : flatten (flatten acc e') e
flatten acc (x :^ e)  = Lambda : symbol x : Dot : flatten acc e
flatten acc (Var x)   = symbol x : acc
flatten acc (Com x)   = symbol x : acc

symbol :: Ident -> Token
symbol v@(Ident x)
    | isUniIdent v = Symbol x
    | otherwise    = LargeSymbol x
