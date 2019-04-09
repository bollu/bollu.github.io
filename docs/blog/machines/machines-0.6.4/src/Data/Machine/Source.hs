{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Source
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank-2 Types
--
----------------------------------------------------------------------------
module Data.Machine.Source
  (
  -- * Sources
    Source, SourceT
  , source
  , repeated
  , cycled
  , cap
  , plug
  , iterated
  , replicated
  , enumerateFromTo
  , unfold
  , unfoldT
  ) where

import Control.Monad.Trans
import Data.Foldable
import Data.Machine.Plan
import Data.Machine.Type
import Data.Machine.Process
import Prelude (Enum, Int, Maybe, Monad, ($), (>>=), return)

-------------------------------------------------------------------------------
-- Source
-------------------------------------------------------------------------------

-- | A 'Source' never reads from its inputs.
type Source b = forall k. Machine k b

-- | A 'SourceT' never reads from its inputs, but may have monadic side-effects.
type SourceT m b = forall k. MachineT m k b

-- | Repeat the same value, over and over.
--
-- This can be constructed from a plan with
-- @
-- repeated :: o -> Source o
-- repeated = repeatedly . yield
-- @
--
-- Examples:
--
-- >>> run $ taking 5 <~ repeated 1
-- [1,1,1,1,1]
--
repeated :: o -> Source o
repeated o =
    loop
  where
    loop = encased (Yield o loop)

-- | Loop through a 'Foldable' container over and over.
--
-- This can be constructed from a plan with
-- @
-- cycled :: Foldable f => f b -> Source b
-- cycled = repeatedly (traverse_ yield xs)
-- @
--
-- Examples:
--
-- >>> run $ taking 5 <~ cycled [1,2]
-- [1,2,1,2,1]
--
cycled :: Foldable f => f b -> Source b
cycled xs = foldr go (cycled xs) xs
  where
    go x m = encased $ Yield x m

-- | Generate a 'Source' from any 'Foldable' container.
--
-- This can be constructed from a plan with
-- @
-- source :: Foldable f => f b -> Source b
-- source = construct (traverse_ yield xs)
-- @
--
-- Examples:
--
-- >>> run $ source [1,2]
-- [1,2]
--
source :: Foldable f => f b -> Source b
source = foldr go stopped
  where
    go x m = encased $ Yield x m

-- |
-- You can transform a 'Source' with a 'Process'.
--
-- Alternately you can view this as capping the 'Source' end of a 'Process',
-- yielding a new 'Source'.
--
-- @'cap' l r = l '<~' r@
--
cap :: Process a b -> Source a -> Source b
cap l r = l <~ r

-- |
-- You can transform any 'MachineT' into a 'SourceT', blocking its input.
--
-- This is used by capT, and capWye, and allows an efficient way to plug
-- together machines of different input languages.
--
plug :: Monad m => MachineT m k o -> SourceT m o
plug (MachineT m) = MachineT $ m >>= \x -> case x of
  Yield o k     -> return (Yield o (plug k))
  Stop          -> return Stop
  Await _ _ h   -> runMachineT $ plug h

-- | 'iterated' @f x@ returns an infinite source of repeated applications
-- of @f@ to @x@
iterated :: (a -> a) -> a -> Source a
iterated f x = construct (go x) where
  go a = do
    yield a
    go (f a)

-- | 'replicated' @n x@ is a source of @x@ emitted @n@ time(s)
replicated :: Int -> a -> Source a
replicated n x = repeated x ~> taking n

-- | Enumerate from a value to a final value, inclusive, via 'succ'
--
-- Examples:
--
-- >>> run $ enumerateFromTo 1 3
-- [1,2,3]
--
enumerateFromTo :: Enum a => a -> a -> Source a
enumerateFromTo start end = source [ start .. end ]

-- | 'unfold' @k seed@ The function takes the element and returns Nothing if it
--   is done producing values or returns Just (a,r), in which case, @a@ is
--   'yield'ed and @r@ is used as the next element in a recursive call.
unfold :: (r -> Maybe (a, r)) -> r -> Source a
unfold k seed = construct (go seed)
  where
    go r = for_ (k r) $ \(a, r') -> do
      yield a
      go r'

-- | Effectful 'unfold' variant.
unfoldT :: Monad m => (r -> m (Maybe (a, r))) -> r -> SourceT m a
unfoldT k seed = construct (go seed)
  where
    go r = do
      opt <- lift $ k r
      for_ opt $ \(a, r') -> do
        yield a
        go r'
