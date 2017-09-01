module Data (
  Ident(..),
  Expr(..),
  Func(..),
  Context,
  emptyContext
 ) where

  import Data.ByteString.Char8 (ByteString)
  import Data.Set (Set)
  import qualified Data.Set      as Set
  import Data.Map.Lazy (Map, (!))
  import qualified Data.Map.Lazy as Map


  data Ident = Ident ByteString
               deriving (Eq, Ord, Show, Read)

  infixl 9 :$
  infixr 7 :^
  data Expr = Expr :$ Expr
              | Ident :^ Expr
              | Var Ident
              deriving (Eq, Show, Read)

  -- | 引数の長さを保持した無名関数
  data Func = Func {
                rank  :: Int
              , alias :: Expr
              }
              deriving (Eq, Show, Read)

  type Context = Map Ident Func

  emptyContext = Map.empty
