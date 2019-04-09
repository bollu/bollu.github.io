{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif
module Data.Machine.Runner
    ( foldrT
    , foldlT
    , foldMapT
    , foldT
    , runT1

    -- Re-exports
    , runT
    , runT_ ) where

import Data.Machine.Type
import Control.Monad (liftM)
#if !MIN_VERSION_base (4,8,0)
import Data.Monoid (Monoid (..))
#endif

-- | Right fold over a stream. This will be lazy if the underlying
-- monad is.
--
-- @runT = foldrT (:) []@
foldrT :: Monad m => (o -> b -> b) -> b -> MachineT m k o -> m b
foldrT c n = go
    where
      go m = do
        step <- runMachineT m
        case step of
          Stop -> return n
          Yield o m' -> c o `liftM` go m'
          Await _ _ m' -> go m'

-- | Strict left fold over a stream.
foldlT :: Monad m => (b -> o -> b) -> b -> MachineT m k o -> m b
foldlT f = go
    where
      go !b m = do
        step <- runMachineT m
        case step of
          Stop -> return b
          Yield o m' -> go (f b o) m'
          Await _ _ m' -> go b m'

-- | Strict fold over a stream. Items are accumulated on the right:
--
-- @... ((f o1 <> f o2) <> f o3) ...@
--
-- Where this is expensive, use the dual monoid instead.
foldMapT :: (Monad m, Monoid r) => (o -> r) -> MachineT m k o -> m r
foldMapT f = foldlT (\b o -> mappend b (f o)) mempty

-- | Strict fold over a monoid stream. Items are accumulated on the
-- right:
--
-- @... ((o1 <> o2) <> o3) ...@
--
-- Where this is expensive, use the dual monoid instead.
--
-- @foldT = foldMapT id@
foldT :: (Monad m, Monoid o) => MachineT m k o -> m o
foldT = foldlT mappend mempty

-- | Run a machine with no input until it yields for the first time,
-- then stop it. This is intended primarily for use with accumulating
-- machines, such as the ones produced by 'fold' or 'fold1'
--
-- @runT1 m = getFirst <$> foldMapT (First . Just) (m ~> taking 1)@
runT1 :: Monad m => MachineT m k o -> m (Maybe o)
runT1 m = do
  step <- runMachineT m
  case step of
    Stop -> return Nothing
    Yield o _ -> return $ Just o
    Await _ _ m' -> runT1 m'
