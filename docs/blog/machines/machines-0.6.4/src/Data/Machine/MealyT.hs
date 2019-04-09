{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Mealy
-- License     :  BSD-style (see the file LICENSE)
--
-- <http://en.wikipedia.org/wiki/Mealy_machine>
-- <https://github.com/ivanperez-keera/dunai/blob/develop/src/Data/MonadicStreamFunction/Core.hs#L35>
-- <https://hackage.haskell.org/package/auto-0.4.3.0/docs/Control-Auto.html>
-- <https://hackage.haskell.org/package/varying-0.6.0.0/docs/Control-Varying-Core.html>
----------------------------------------------------------------------------
module Data.Machine.MealyT
  ( MealyT(..)
  , arrPure
  , arrM
  , upgrade
  , scanMealyT
  , scanMealyTM
  , embedMealyT
  ) where

import Data.Machine
import Control.Arrow
import Control.Applicative
import Data.Pointed
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Profunctor
import Data.Semigroup
import qualified Control.Category as C
import Prelude

-- | 'Mealy' machine, with monadic effects
newtype MealyT m a b = MealyT { runMealyT :: a -> m (b, MealyT m a b) }

instance Functor m => Functor (MealyT m a) where
  {-# INLINE fmap #-}
  fmap f (MealyT m) = MealyT $ \a ->
    fmap (\(x,y) -> (f x, fmap f y)) (m a)

instance Pointed m => Pointed (MealyT m a) where
  {-# INLINE point #-}
  point b = r where r = MealyT (const (point (b, r)))

instance Applicative m => Applicative (MealyT m a) where
  {-# INLINE pure #-}
  pure b = r where r = MealyT (const (pure (b, r))) -- Stolen from Pointed
  MealyT m <*> MealyT n = MealyT $ \a -> (\(mb, mm) (nb, nm) -> (mb nb, mm <*> nm)) <$> m a <*> n a

instance Monad m => Monad (MealyT m a) where
#if !MIN_VERSION_base(4,8,0)
  -- pre-AMP
  {-# INLINE return #-}
  return b = r where r = MealyT (const (return (b, r))) -- Stolen from Pointed
#endif

  MealyT g >>= f = MealyT $ \a ->
    do (b, MealyT _h) <- g a
       runMealyT (f b) a

-- | Profunctor Example:
--
-- >>> embedMealyT (dimap (+21) (+1) (arr (+1))) [1,2,3 :: Int]
-- [24,25,26]
--
instance Functor m => Profunctor (MealyT m) where
  rmap = fmap
  {-# INLINE rmap #-}
  lmap f = go where
    go (MealyT m) = MealyT $ \a -> fmap (\(b,n) -> (b, go n)) (m (f a))
  {-# INLINE lmap #-}
#if MIN_VERSION_profunctors(3,1,1)
  dimap f g = go where
    go (MealyT m) = MealyT $ \a -> fmap (\(b,n) -> (g b, go n)) (m (f a))
  {-# INLINE dimap #-}
#endif

instance Monad m => C.Category (MealyT m) where
  {-# INLINE id #-}
  id = MealyT $ \a -> return (a, C.id)
  MealyT bc . MealyT ab = MealyT $ \a ->
    do (b, nab) <- ab a
       (c, nbc) <- bc b
       return (c, nbc C.. nab)

instance Monad m => Arrow (MealyT m) where
  {-# INLINE arr #-}
  arr f = r where r = MealyT (\a -> return (f a, r))
  first (MealyT m) = MealyT $ \(a,c) ->
    do (b, n) <- m a
       return ((b, c), first n)

arrPure :: (a -> b) -> MealyT Identity a b
arrPure = arr

arrM :: Functor m => (a -> m b) -> MealyT m a b
arrM f = r where r = MealyT $ \a -> fmap (,r) (f a)

upgrade :: Monad m => Mealy a b -> MealyT m a b
upgrade (Mealy f) = MealyT $ \a ->
  do let (r, g) = f a
     return (r, upgrade g)

scanMealyT :: Monad m => (a -> b -> a) -> a -> MealyT m b a
scanMealyT f a = MealyT (\b -> return (a, scanMealyT f (f a b)))

scanMealyTM :: Functor m => (a -> b -> m a) -> a -> MealyT m b a
scanMealyTM f a = MealyT $ \b -> (\x -> (a, scanMealyTM f x)) <$> f a b

autoMealyTImpl :: Monad m => MealyT m a b -> ProcessT m a b
autoMealyTImpl = construct . go
  where
  go (MealyT f) = do
    a      <- await
    (b, m) <- lift $ f a
    yield b
    go m

-- | embedMealyT Example:
--
-- >>> embedMealyT (arr (+1)) [1,2,3]
-- [2,3,4]
--
embedMealyT :: Monad m => MealyT m a b -> [a] -> m [b]
embedMealyT _  []     = return []
embedMealyT sf (a:as) = do
  (b, sf') <- runMealyT sf a
  bs       <- embedMealyT sf' as
  return (b:bs)

instance AutomatonM MealyT where
  autoT = autoMealyTImpl

instance (Semigroup b, Monad m) => Semigroup (MealyT m a b) where
  f <> g = MealyT $ \x -> do
    (fx, f') <- runMealyT f x
    (gx, g') <- runMealyT g x
    return (fx <> gx, f' <> g')

instance (Monoid b, Monad m) => Monoid (MealyT m a b) where
  mempty = MealyT $ \_ -> return mempty
  mappend f g = MealyT $ \x -> do
    (fx, f') <- runMealyT f x
    (gx, g') <- runMealyT g x
    return (fx `mappend` gx, f' `mappend` g')
