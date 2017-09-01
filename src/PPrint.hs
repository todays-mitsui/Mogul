{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module PPrint (pp) where


import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString, unpack)
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
  prepara = prepara . alias

instance PPrintable (Ident, Func) where
  prepara (v, f) = symbol v : Equal : prepara f

instance PPrintable Context where
  prepara = foldrWithKey (\v f done -> prepara (v, f) <> (EOL : done)) []

--------------------------------------------------------------------------------

data Phrase = Backquote                 -- "`"
              | Lambda                  -- "^"
              | Dot                     -- "."
              | Equal                   -- "="
              | Symbol ByteString       -- 英小文字1文字からなるシンボル
              | LargeSymbol ByteString  -- 英数字2文字以上からなるシンボル
              | EOL                     -- 行端
              deriving (Eq, Show)

--------------------------------------------------------------------------------

-- prepara :: Expr -> [Phrase]
-- prepara = prepara' []

prepara' :: [Phrase] -> Expr -> [Phrase]
prepara' acc (e :$ e') = Backquote : prepara' (prepara' acc e') e
prepara' acc (x :^ e)  = Lambda : symbol x : Dot : prepara' acc e
prepara' acc (Var x)   = symbol x : acc


symbol :: Ident -> Phrase
symbol (Ident x)
  | 1 == BS.length x && BS.head x `BS.elem` lowers = Symbol x
  | otherwise                                      = LargeSymbol x
  where lowers = "abcdefghijklmnopqrstuvwxyz"

--------------------------------------------------------------------------------

render :: [Phrase] -> String
render = unpack . render'

render' :: [Phrase] -> ByteString
render' []        = ""
render' (LargeSymbol s:ps@(LargeSymbol _:_))
                  = s <> " " <> render' ps
render' (p:ps)    = byteStringShow p <> render' ps

--------------------------------------------------------------------------------

byteStringShow :: Phrase -> ByteString
byteStringShow Backquote       = "`"
byteStringShow Lambda          = "^"
byteStringShow Dot             = "."
byteStringShow Equal           = "="
byteStringShow (Symbol s)      = s
byteStringShow (LargeSymbol s) = s
byteStringShow EOL             = "\n"
