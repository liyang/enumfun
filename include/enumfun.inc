{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Finitely represented /total/ EnumMaps. Comprises a partial EnumMap and
a default value. Has Applicative and Monad instances, unlike EnumMap.
Now with extra lensy goodness.

Inspired by Conal's Data.TotalMap:
<http://hackage.haskell.org/package/total-map>
-}

module Data.EnumFun.LAZY
    ( EnumFun (..)
    , (!)
    , trim

    -- * Lenses
    , val

    -- * List Conversion
    , fromList
    , from/**/STRICT
    ) where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Lens
import qualified Data.EnumMap.LAZY as EnumMap
import Data.EnumFun.LAZY.Base
import qualified Data.EnumFun.STRICT.Base as STRICT
import Data.Monoid

instance (Enum k) => Applicative (EnumFun k) where
    pure v = EnumFun v mempty
    EnumFun df mf <*> EnumFun dv mv = EnumFun (df dv) $
        EnumMap.mergeWithKey (\ _ f v -> Just (f v))
            (fmap ($ dv)) (fmap df) mf mv

instance (Enum k, Monoid v) => Monoid (EnumFun k v) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance (Enum k) => Monad (EnumFun k) where
    return = pure
    m >>= f = mu (f <$> m) where
        mu :: (Enum k) => EnumFun k (EnumFun k v) -> EnumFun k v
        mu (EnumFun (EnumFun dd dm) mf) = EnumFun dd $
            EnumMap.mapWithKey (flip (!)) mf `EnumMap.union` dm

-- | Sample a total map. Semantic function.
(!) :: (Enum k) => EnumFun k v -> k -> v
EnumFun d m ! k = EnumMap.findWithDefault d k m

-- | Optimise an 'EnumFun', weeding out any explicit default values.
-- A semantic no-op, i.e., @(!) . trim == (!)@.
trim :: (Eq v) => EnumFun k v -> EnumFun k v
trim (EnumFun d m) = EnumFun d $ EnumMap.filter (d /=) m

------------------------------------------------------------------------

-- | An 'IndexedLens' for 'EnumFun', where 'Nothing' selects the default
-- value. If the value being set is equal to the default, it is 'trim'med
-- from the partial map.
val :: (Enum k, Eq v) => Maybe k -> SimpleIndexedLens (Maybe k) (EnumFun k v) v
val mk = index $ \ g f@(EnumFun d m) -> case mk of
    Nothing -> flip EnumFun m <$> g mk d
    Just k -> EnumFun d . m' <$> g mk (f ! k) where
        m' v = if v == d then EnumMap.delete k m else EnumMap.insert k v m

------------------------------------------------------------------------

-- | Create an 'EnumFun' from a default value and a list of key/value pairs.
fromList :: (Enum k) => v -> [(k, v)] -> EnumFun k v
fromList d = EnumFun d . EnumMap.fromList

-- | Convert from a STRICT EnumFun. The operation is essentially free; we
-- only needed two distinct types for the different class instances.
from/**/STRICT :: STRICT.EnumFun k v -> EnumFun k v
from/**/STRICT (STRICT.EnumFun d m) = EnumFun d m

-- vim: ft=haskell:

