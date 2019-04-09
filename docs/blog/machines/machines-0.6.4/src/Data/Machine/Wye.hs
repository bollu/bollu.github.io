{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Wye
-- Copyright   :  (C) 2012 Edward Kmett, Rúnar Bjarnason, Paul Chiusano
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank-2 Types, GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Wye
  (
  -- * Wyes
    Wye, WyeT
  , Y(..)
  , wye
  , addX, addY
  , capX, capY, capWye
  ) where

import Control.Category
import Data.Machine.Process
import Data.Machine.Type
import Data.Machine.Is
import Data.Machine.Source
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Wyes
-------------------------------------------------------------------------------

-- | The input descriptor for a 'Wye' or 'WyeT'
data Y a b c where
  X :: Y a b a            -- block waiting on the left input
  Y :: Y a b b            -- block waiting on the right input
  Z :: Y a b (Either a b) -- block waiting on either input

-- | A 'Machine' that can read from two input stream in a non-deterministic manner.
type Wye a b c = Machine (Y a b) c

-- | A 'Machine' that can read from two input stream in a non-deterministic manner with monadic side-effects.
type WyeT m a b c = MachineT m (Y a b) c

-- | Compose a pair of pipes onto the front of a 'Wye'.

-- | Precompose a 'Process' onto each input of a 'Wye' (or 'WyeT').
--
-- This is left biased in that it tries to draw values from the 'X' input whenever they are
-- available, and only draws from the 'Y' input when 'X' would block.
wye :: Monad m => ProcessT m a a' -> ProcessT m b b' -> WyeT m a' b' c -> WyeT m a b c
wye ma mb m = MachineT $ runMachineT m >>= \v -> case v of
  Yield o k           -> return $ Yield o (wye ma mb k)
  Stop                -> return Stop
  Await f X ff        -> runMachineT ma >>= \u -> case u of
    Yield a k           -> runMachineT . wye k mb $ f a
    Stop                -> runMachineT $ wye stopped mb ff
    Await g Refl fg     -> return . Await (\a -> wye (g a) mb $ encased v) X
                                  . wye fg mb $ encased v
  Await f Y ff        -> runMachineT mb >>= \u -> case u of
    Yield b k           -> runMachineT . wye ma k $ f b
    Stop                -> runMachineT $ wye ma stopped ff
    Await g Refl fg     -> return . Await (\b -> wye ma (g b) $ encased v) Y
                                  . wye ma fg $ encased v
  Await f Z ff        -> runMachineT ma >>= \u -> case u of
    Yield a k           -> runMachineT . wye k mb . f $ Left a
    Stop                -> runMachineT mb >>= \w -> case w of
      Yield b k           -> runMachineT . wye stopped k . f $ Right b
      Stop                -> runMachineT $ wye stopped stopped ff
      Await g Refl fg     -> return . Await (\b -> wye stopped (g b) $ encased v) Y
                                    . wye stopped fg $ encased v
    Await g Refl fg     -> runMachineT mb >>= \w -> case w of
      Yield b k           -> runMachineT . wye (encased u) k . f $ Right b
      Stop                -> return . Await (\a -> wye (g a) stopped $ encased v) X
                                    . wye fg stopped $ encased v
      Await h Refl fh     -> return . Await (\c -> case c of
                                                  Left a  -> wye (g a) (encased w) $ encased v
                                                  Right b -> wye (encased u) (h b) $ encased v) Z
                                    . wye fg fh $ encased v

-- | Precompose a pipe onto the left input of a wye.
addX :: Monad m => ProcessT m a b -> WyeT m b c d -> WyeT m a c d
addX p = wye p echo
{-# INLINE addX #-}

-- | Precompose a pipe onto the right input of a wye.
addY :: Monad m => ProcessT m b c -> WyeT m a c d -> WyeT m a b d
addY = wye echo
{-# INLINE addY #-}

-- | Tie off one input of a wye by connecting it to a known source.
capX :: Monad m => SourceT m a -> WyeT m a b c -> ProcessT m b c
capX s t = process (capped Right) (addX s t)
{-# INLINE capX #-}

-- | Tie off one input of a wye by connecting it to a known source.
capY :: Monad m => SourceT m b -> WyeT m a b c -> ProcessT m a c
capY s t = process (capped Left) (addY s t)
{-# INLINE capY #-}

-- | Tie off both inputs of a wye by connecting them to known sources.
capWye :: Monad m => SourceT m a -> SourceT m b -> WyeT m a b c -> SourceT m c
capWye a b = plug . wye a b
{-# INLINE capWye #-}

-- | Natural transformation used by 'capX' and 'capY'
capped :: (a -> Either a a) -> Y a a b -> a -> b
capped _ X = id
capped _ Y = id
capped f Z = f
{-# INLINE capped #-}
