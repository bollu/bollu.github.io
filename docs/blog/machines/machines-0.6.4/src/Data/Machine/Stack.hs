{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Stack
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Stack
  ( Stack(..)
  , stack
  , peek
  , pop
  , push
  ) where

import Data.Machine.Plan
import Data.Machine.Type

-- | This is a simple process type that knows how to push back input.
data Stack a r where
  Push :: a -> Stack a ()
  Pop  ::      Stack a a

-- | Peek at the next value in the input stream without consuming it
peek :: Plan (Stack a) b a
peek = do
  a <- pop
  push a
  return a
{-# INLINABLE peek #-}

-- | Push back into the input stream
push :: a -> Plan (Stack a) b ()
push a = awaits (Push a)
{-# INLINABLE push #-}

-- | Pop the next value in the input stream
pop :: Plan (Stack a) b a
pop = awaits Pop
{-# INLINABLE pop #-}

-- | Stream outputs from one 'Machine' into another with the possibility
-- of pushing inputs back.
stack :: Monad m => MachineT m k a -> MachineT m (Stack a) o -> MachineT m k o
stack up down =
  stepMachine down $ \stepD     ->
  case stepD of
    Stop                     -> stopped
    Yield o down'            -> encased (Yield o (up `stack` down'))
    Await down' (Push a) _   -> encased (Yield a up) `stack` down' ()
    Await down' Pop ffD      ->
      stepMachine up $ \stepU   ->
      case stepU of
        Stop                 -> stopped `stack` ffD
        Yield o up'          -> up'     `stack` down' o
        Await up' req ffU    -> encased (Await (\a -> up' a `stack` encased stepD) req
                                               (      ffU   `stack` encased stepD))
{-# INLINABLE stack #-}
