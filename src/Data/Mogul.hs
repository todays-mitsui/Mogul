module Data.Mogul
    ( Mogul
    ) where

import Control.Monad.State.Lazy (StateT)

import Data.Context


type Mogul a = StateT Context IO a
