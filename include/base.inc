{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | 'EnumFun' internals.
module Data.EnumFun.LAZY.Base
    ( EnumFun (..)
    ) where

import Data.EnumMap.LAZY (EnumMap)
import Data.Foldable
import Data.Traversable
import Data.Typeable

-- | Total EnumMap.
data EnumFun k v = EnumFun BANG/**/v BANG(EnumMap k v)
    -- ^ Default value and a finite map.
    deriving (Eq, Functor, Foldable, Ord, Show, Traversable, Typeable)

-- vim: ft=haskell:

