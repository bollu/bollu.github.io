{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#ifndef MIN_VERSION_profunctors
#define MIN_VERSION_profunctors(x,y,z) 0
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Moore
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- <http://en.wikipedia.org/wiki/Moore_machine>
----------------------------------------------------------------------------
module Data.Machine.Moore
  ( Moore(..)
  , logMoore
  , unfoldMoore
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad.Fix
import Control.Monad.Reader.Class
import Control.Monad.Zip
import Data.Copointed
import Data.Distributive
import Data.Functor.Rep as Functor
import Data.Machine.Plan
import Data.Machine.Type
import Data.Machine.Process
import Data.Monoid
import Data.Pointed
import Data.Profunctor.Closed
import Data.Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Rep as Profunctor
import Prelude

-- | 'Moore' machines
data Moore a b = Moore b (a -> Moore a b)

-- | Accumulate the input as a sequence.
logMoore :: Monoid m => Moore m m
logMoore = h mempty where
  h m = Moore m (\a -> h (m <> a))
{-# INLINE logMoore #-}

-- | Construct a Moore machine from a state valuation and transition function
unfoldMoore :: (s -> (b, a -> s)) -> s -> Moore a b
unfoldMoore f = go where
  go s = case f s of
    (b, g) -> Moore b (go . g)
{-# INLINE unfoldMoore #-}

instance Automaton Moore where
  auto = construct . go where
    go (Moore b f) = do
      yield b
      await >>= go . f
  {-# INLINE auto #-}

instance Functor (Moore a) where
  fmap f (Moore b g) = Moore (f b) (fmap f . g)
  {-# INLINE fmap #-}
  a <$ _ = return a
  {-# INLINE (<$) #-}

instance Profunctor Moore where
  rmap = fmap
  {-# INLINE rmap #-}
  lmap f = go where
    go (Moore b g) = Moore b (go . g . f)
  {-# INLINE lmap #-}
#if MIN_VERSION_profunctors(3,1,1)
  dimap f g = go where
    go (Moore b h) = Moore (g b) (go . h . f)
  {-# INLINE dimap #-}
#endif

instance Applicative (Moore a) where
  pure a = r where r = Moore a (const r)
  {-# INLINE pure #-}
  Moore f ff <*> Moore a fa  = Moore (f a) (\i -> ff i <*> fa i)
  m <* _ = m
  {-# INLINE (<*) #-}
  _ *> n = n
  {-# INLINE (*>) #-}

instance Pointed (Moore a) where
  point a = r where r = Moore a (const r)
  {-# INLINE point #-}

-- | slow diagonalization
instance Monad (Moore a) where
  return = pure
  {-# INLINE return #-}
  k >>= f = j (fmap f k) where
    j (Moore a g) = Moore (extract a) (\x -> j $ fmap (\(Moore _ h) -> h x) (g x))
  (>>) = (*>)

instance Copointed (Moore a) where
  copoint (Moore b _) = b
  {-# INLINE copoint #-}

instance Comonad (Moore a) where
  extract (Moore b _) = b
  {-# INLINE extract #-}
  extend f w@(Moore _ g) = Moore (f w) (extend f . g)

instance ComonadApply (Moore a) where
  Moore f ff <@> Moore a fa = Moore (f a) (\i -> ff i <@> fa i)
  m <@ _ = m
  {-# INLINE (<@) #-}
  _ @> n = n
  {-# INLINE (@>) #-}

instance Distributive (Moore a) where
  distribute m = Moore (fmap extract m) (distribute . collect (\(Moore _ k) -> k) m)

instance Functor.Representable (Moore a) where
  type Rep (Moore a) = [a]
  index = cosieve
  tabulate = cotabulate
  {-# INLINE tabulate #-}

instance Cosieve Moore [] where
  cosieve (Moore b _) [] = b
  cosieve (Moore _ k) (a:as) = cosieve (k a) as

instance Costrong Moore where
  unfirst = unfirstCorep
  unsecond = unsecondCorep

instance Profunctor.Corepresentable Moore where
  type Corep Moore = []
  cotabulate f0 = go (f0 . reverse) where
    go f = Moore (f []) $ \a -> go (f.(a:))

instance MonadFix (Moore a) where
  mfix = mfixRep

instance MonadZip (Moore a) where
  mzipWith = mzipWithRep
  munzip m = (fmap fst m, fmap snd m)

instance MonadReader [a] (Moore a) where
  ask = askRep
  local = localRep

instance Closed Moore where
  closed m = cotabulate $ \fs x -> cosieve m (fmap ($x) fs)
