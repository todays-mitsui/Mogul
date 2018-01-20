module Data.Ident (
      Ident(..)
    , isUniIdent
    , isLargeIdent
    ) where


import qualified Data.Text as T
import Data.Text              (Text)
import Data.Char              (isLower)

import Data.Aeson


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

instance ToJSON Ident where
    toJSON (Ident x) = toJSON x
