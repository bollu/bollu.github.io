{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#ifndef MIN_VERSION_profunctors
#define MIN_VERSION_profunctors(x,y,z) 0
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Mealy
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- <http://en.wikipedia.org/wiki/Mealy_machine>
----------------------------------------------------------------------------
module Data.Machine.Mealy
  ( Mealy(..)
  , unfoldMealy
  , logMealy
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Fix
import Control.Monad.Reader.Class
import Control.Monad.Zip
import Data.Distributive
import Data.Functor.Extend
import Data.Functor.Rep as Functor
import Data.List.NonEmpty as NonEmpty
import Data.Machine.Plan
import Data.Machine.Type
import Data.Machine.Process
import Data.Profunctor.Closed
import Data.Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Rep as Profunctor
import Data.Pointed
import Data.Semigroup
import Data.Sequence as Seq
import Prelude hiding ((.),id)

-- $setup
-- >>> import Data.Machine.Source

-- | 'Mealy' machines
--
-- ==== Examples
--
-- We can enumerate inputs:
--
-- >>> let countingMealy = unfoldMealy (\i x -> ((i, x), i + 1)) 0
-- >>> run (auto countingMealy <~ source "word")
-- [(0,'w'),(1,'o'),(2,'r'),(3,'d')]
--
newtype Mealy a b = Mealy { runMealy :: a -> (b, Mealy a b) }

instance Functor (Mealy a) where
  fmap f (Mealy m) = Mealy $ \a -> case m a of
    (b, n) -> (f b, fmap f n)
  {-# INLINE fmap #-}
  b <$ _ = pure b
  {-# INLINE (<$) #-}

instance Applicative (Mealy a) where
  pure b = r where r = Mealy (const (b, r))
  {-# INLINE pure #-}
  Mealy m <*> Mealy n = Mealy $ \a -> case m a of
    (f, m') -> case n a of
       (b, n') -> (f b, m' <*> n')
  m <* _ = m
  {-# INLINE (<*) #-}
  _ *> n = n
  {-# INLINE (*>) #-}

instance Pointed (Mealy a) where
  point b = r where r = Mealy (const (b, r))
  {-# INLINE point #-}

instance Extend (Mealy a) where
  duplicated (Mealy m) = Mealy $ \a -> case m a of
    (_, b) -> (b, duplicated b)

-- | A 'Mealy' machine modeled with explicit state.
unfoldMealy :: (s -> a -> (b, s)) -> s -> Mealy a b
unfoldMealy f = go where
  go s = Mealy $ \a -> case f s a of
    (b, t) -> (b, go t)
{-# INLINE unfoldMealy #-}

-- | slow diagonalization
instance Monad (Mealy a) where
  return = pure
  {-# INLINE return #-}
  m >>= f = Mealy $ \a -> case runMealy m a of
    (b, m') -> (fst (runMealy (f b) a), m' >>= f)
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}

instance Profunctor Mealy where
  rmap = fmap
  {-# INLINE rmap #-}
  lmap f = go where
    go (Mealy m) = Mealy $ \a -> case m (f a) of
      (b, n) -> (b, go n)
  {-# INLINE lmap #-}
#if MIN_VERSION_profunctors(3,1,1)
  dimap f g = go where
    go (Mealy m) = Mealy $ \a -> case m (f a) of
      (b, n) -> (g b, go n)
  {-# INLINE dimap #-}
#endif

instance Automaton Mealy where
  auto = construct . go where
    go (Mealy f) = await >>= \a -> case f a of
      (b, m) -> do
         yield b
         go m
  {-# INLINE auto #-}

instance Category Mealy where
  id = Mealy (\a -> (a, id))
  Mealy bc . Mealy ab = Mealy $ \ a -> case ab a of
    (b, nab) -> case bc b of
      (c, nbc) -> (c, nbc . nab)

instance Arrow Mealy where
  arr f = r where r = Mealy (\a -> (f a, r))
  {-# INLINE arr #-}
  first (Mealy m) = Mealy $ \(a,c) -> case m a of
    (b, n) -> ((b, c), first n)

instance ArrowChoice Mealy where
  left m = Mealy $ \a -> case a of
    Left l  -> case runMealy m l of
      (b, m') -> (Left b, left m')
    Right r -> (Right r, left m)
  right m = Mealy $ \a -> case a of
    Left l -> (Left l, right m)
    Right r -> case runMealy m r of
      (b, m') -> (Right b, right m')
  m +++ n = Mealy $ \a -> case a of
    Left b -> case runMealy m b of
      (c, m') -> (Left c, m' +++ n)
    Right b -> case runMealy n b of
      (c, n') -> (Right c, m +++ n')
  m ||| n = Mealy $ \a -> case a of
    Left b -> case runMealy m b of
      (d, m') -> (d, m' ||| n)
    Right b -> case runMealy n b of
      (d, n') -> (d, m ||| n')

#if MIN_VERSION_profunctors(3,2,0)
instance Strong Mealy where
  first' = first

instance Choice Mealy where
  left' = left
  right' = right
#endif

-- | Fast forward a mealy machine forward
driveMealy :: Mealy a b -> Seq a -> a -> (b, Mealy a b)
driveMealy m xs z = case viewl xs of
  y :< ys -> case runMealy m y of
    (_, n) -> driveMealy n ys z
  EmptyL  -> runMealy m z

-- | Accumulate history.
logMealy :: Semigroup a => Mealy a a
logMealy = Mealy $ \a -> (a, h a) where
  h a = Mealy $ \b -> let c = a <> b in (c, h c)
{-# INLINE logMealy #-}

instance ArrowApply Mealy where
  app = go Seq.empty where
    go xs = Mealy $ \(m,x) -> case driveMealy m xs x of
      (c, _) -> (c, go (xs |> x))
  {-# INLINE app #-}

instance Distributive (Mealy a) where
  distribute fm = Mealy $ \a -> let fp = fmap (`runMealy` a) fm in
     (fmap fst fp, collect snd fp)
  collect k fa = Mealy $ \a -> let fp = fmap (\x -> runMealy (k x) a) fa in
     (fmap fst fp, collect snd fp)

instance Functor.Representable (Mealy a) where
  type Rep (Mealy a) = NonEmpty a
  index = cosieve
  tabulate = cotabulate

instance Cosieve Mealy NonEmpty where
  cosieve m0 (a0 :| as0) = go m0 a0 as0 where
    go (Mealy m) a as = case m a of
      (b, m') -> case as of
        [] -> b
        a':as' -> go m' a' as'

instance Costrong Mealy where
  unfirst = unfirstCorep
  unsecond = unsecondCorep

instance Profunctor.Corepresentable Mealy where
  type Corep Mealy = NonEmpty
  cotabulate f0 = Mealy $ \a -> go [a] f0 where
     go as f = (f (NonEmpty.fromList (Prelude.reverse as)), Mealy $ \b -> go (b:as) f)

instance MonadFix (Mealy a) where
  mfix = mfixRep

instance MonadZip (Mealy a) where
  mzipWith = mzipWithRep
  munzip m = (fmap fst m, fmap snd m)

instance MonadReader (NonEmpty a) (Mealy a) where
  ask = askRep
  local = localRep

instance Closed Mealy where
  closed m = cotabulate $ \fs x -> cosieve m (fmap ($x) fs)

instance Semigroup b => Semigroup (Mealy a b) where
  f <> g = Mealy $ \x -> runMealy f x <> runMealy g x

instance Monoid b => Monoid (Mealy a b) where
  mempty = Mealy mempty
  mappend f g = Mealy $ \x -> runMealy f x `mappend` runMealy g x
