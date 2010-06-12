-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Instances
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Vector.Instances () where

import Prelude hiding ((++))
import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Data.Monoid
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Stream as Stream
import Data.Vector.Fusion.Stream.Size
import Data.Vector (Vector, (++))

instance Functor Vector where
    fmap = Vector.map
    {-# INLINE fmap #-}

instance Monad Vector where
    return = Vector.singleton
    {-# INLINE return #-}
    v >>= f = Vector.concatMap f v
    {-# INLINE (>>=) #-}
    fail _ = Vector.empty
    {-# INLINE fail #-}

instance MonadPlus Vector where
    mzero = Vector.empty
    {-# INLINE mzero #-}
    mplus = (++)
    {-# INLINE mplus #-}

instance Monoid (Vector a) where
    mempty = Vector.empty
    {-# INLINE mempty #-}
    mappend = (++)
    {-# INLINE mappend #-}

instance Applicative Vector where
    pure = Vector.singleton
    {-# INLINE pure #-}
    fs <*> as =
        G.unstream $ Stream.sized results (Exact n)
        where
            n = Vector.length fs * Vector.length as
            results = Stream.concatMap body $ G.stream fs
            body f = Stream.map f $ G.stream as
    {-# INLINE (<*>) #-}

instance Alternative Vector where
    (<|>) = (++)
    {-# INLINE (<|>) #-}
    empty = Vector.empty
    {-# INLINE empty #-}

instance Foldable Vector where
    foldl = Vector.foldl
    {-# INLINE foldl #-}
    foldr = Vector.foldr
    {-# INLINE foldr #-}
    foldl1 = Vector.foldl1
    {-# INLINE foldl1 #-}
    foldr1 = Vector.foldr1
    {-# INLINE foldr1 #-}

instance Traversable Vector where
    traverse f v
        = Vector.fromListN (Vector.length v) <$> traverse f (Vector.toList v)
    {-# INLINE traverse #-}
