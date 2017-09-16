{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module PPrint (pp) where


import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Data.Map.Lazy (foldrWithKey)

import Data
import Expr

class PPrintable a where
  prepara :: a -> [Phrase]
  pp      :: a -> String
  pp = render . prepara

instance PPrintable Ident where
  prepara = (:[]) . symbol

instance PPrintable Expr where
  prepara = prepara' []

instance PPrintable Func where
  prepara (Func args e) = prepara $ foldl (flip (:^)) e args

instance PPrintable (Ident, Func) where
  prepara (v, f) = symbol v : Equal : prepara f

instance PPrintable Context where
  prepara = foldrWithKey (\v f done -> prepara (v, f) <> (EOL : done)) []

--------------------------------------------------------------------------------

data Phrase = Backquote           -- "`"
              | Lambda            -- "^"
              | Dot               -- "."
              | Equal             -- "="
              | Symbol Text       -- 英小文字1文字からなるシンボル
              | LargeSymbol Text  -- 英数字2文字以上からなるシンボル
              | EOL               -- 行端
              deriving (Eq, Show)

--------------------------------------------------------------------------------

-- prepara :: Expr -> [Phrase]
-- prepara = prepara' []

prepara' :: [Phrase] -> Expr -> [Phrase]
prepara' acc (e :$ e') = Backquote : prepara' (prepara' acc e') e
prepara' acc (x :^ e)  = Lambda : symbol x : Dot : prepara' acc e
prepara' acc (Var x)   = symbol x : acc


symbol :: Ident -> Phrase
symbol (UniIdent   x)          = Symbol x
symbol (LargeIdent x Nothing ) = LargeSymbol x
symbol (LargeIdent x (Just n)) = LargeSymbol (x <> pack (show n))

--------------------------------------------------------------------------------

render :: [Phrase] -> String
render = unpack . render'

render' :: [Phrase] -> Text
render' []        = ""
render' (LargeSymbol s:ps@(LargeSymbol _:_))
                  = s <> " " <> render' ps
render' (p:ps)    = textShow p <> render' ps

--------------------------------------------------------------------------------

textShow :: Phrase -> Text
textShow Backquote       = "`"
textShow Lambda          = "^"
textShow Dot             = "."
textShow Equal           = "="
textShow (Symbol s)      = s
textShow (LargeSymbol s) = s
textShow EOL             = "\n"
