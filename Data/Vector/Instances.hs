{-# LANGUAGE TypeFamilies #-}
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

import Prelude hiding ((++), drop, length)
import Control.Applicative
import Control.Monad
import Data.Semigroup
import Data.Key
import Data.Functor.Bind
import Data.Functor.Extend
import Data.Functor.Plus
import Data.Pointed
import Data.Monoid
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Stream as Stream
import Data.Vector.Fusion.Stream.Size
import Data.Vector (Vector,(++),drop,length,imap,ifoldr, ifoldl, izipWith,(!?),(//), generate)
import qualified Data.Vector as Vector

type instance Key Vector = Int

instance Keyed Vector where
  mapWithKey = imap
  {-# INLINE mapWithKey #-}

instance Zip Vector where
  zipWith = Vector.zipWith
  {-# INLINE zipWith #-}

instance ZipWithKey Vector where
  zipWithKey = Vector.izipWith
  {-# INLINE zipWithKey #-}

instance Indexable Vector where
  index = (!)
  {-# INLINE index #-}

instance Lookup Vector where
  lookup = flip (!?) 
  {-# INLINE lookup #-}

instance Adjustable Vector where
  adjust f n v = case v !? n of
    Just a ->  v // [(n, f a)]
    Nothing -> v
  {-# INLINE adjust #-}

  replace n a v = v // [(n,a)]
  {-# INLINE replace #-}

instance FoldableWithKey Vector where
  foldrWithKey = ifoldr
  {-# INLINE foldrWithKey #-}
  foldlWithKey = ifoldl
  {-# INLINE foldlWithKey #-}

instance Apply Vector where
  fs <.> as =
    G.unstream $ Stream.sized results (Exact n)
    where
      n = Vector.length fs * Vector.length as
      results = Stream.concatMap body $ G.stream fs
      body f = Stream.map f $ G.stream as
  {-# INLINE (<.>) #-}

instance Pointed Vector where
  point = Vector.singleton
  {-# INLINE point #-}

instance Bind Vector where
  v >>- f = Vector.concatMap f v
  {-# INLINE (>>-) #-}

instance Semigroup (Vector a) where
  (<>) = (++)
  {-# INLINE (<>) #-}
  
instance Alt Vector where
  (<!>) = (++)
  {-# INLINE (<!>) #-}

instance Plus Vector where
  zero = Vector.empty
  {-# INLINE zero #-}

instance TraversableWithKey Vector where
  traverseWithKey f v
     = Vector.fromListN (Vector.length v) <$> traverseWithKey f (Vector.toList v)
  {-# INLINE traverseWithKey #-}

instance Extend Vector where
  duplicated v = generate (length v) (`drop` v)
  {-# INLINE duplicated #-}
  extended f v = generate (length v) (\n -> f (drop n v))
  {-# INLINE extended #-}


