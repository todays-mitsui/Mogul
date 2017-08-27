{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module PPrint (pp) where


import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString, unpack)

import Expr

class PPrintable a where
  pp :: a -> String

instance PPrintable Ident where
  pp (Ident bs) = unpack bs

instance PPrintable Expr where
  pp = render . prepara

instance PPrintable Function where
  pp = pp . alias

instance PPrintable (Ident, Function) where
  pp (v, f) = pp v <> " = " <> pp f

--------------------------------------------------------------------------------

data Phrase = Backquote
              | Lambda
              | Dot
              | Symbol ByteString
              | LargeSymbol ByteString
              deriving (Eq, Show)

--------------------------------------------------------------------------------

prepara :: Expr -> [Phrase]
prepara = prepara' []

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
byteStringShow (Symbol s)      = s
byteStringShow (LargeSymbol s) = s
