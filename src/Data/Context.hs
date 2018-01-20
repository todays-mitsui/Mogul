module Data.Context
    ( Context
    , emptyContext
    ) where


import qualified Data.Map.Lazy as Map
import Data.Map.Lazy              (Map, (!))

import Data.Ident
import Data.Func


-- | 関数定義の集合
type Context = Map Ident Func

emptyContext = Map.empty
