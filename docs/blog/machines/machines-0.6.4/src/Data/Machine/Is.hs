{-# LANGUAGE GADTs, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Is
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  GADTs, Type Families
--
----------------------------------------------------------------------------
module Data.Machine.Is
  ( Is(..)
  ) where

import Control.Category
import Data.Semigroup
import Prelude

-- | Witnessed type equality
data Is a b where
  Refl :: Is a a

instance Show (Is a b) where
  showsPrec _ Refl = showString "Refl"

instance Eq (Is a b) where
  Refl == Refl = True
  {-# INLINE (==) #-}

instance Ord (Is a b) where
  Refl `compare` Refl = EQ
  {-# INLINE compare #-}

instance (a ~ b) => Semigroup (Is a b) where
  Refl <> Refl = Refl
  {-# INLINE (<>) #-}

instance (a ~ b) => Monoid (Is a b) where
  mempty = Refl
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance (a ~ b) => Read (Is a b) where
  readsPrec d = readParen (d > 10) (\r -> [(Refl,s) | ("Refl",s) <- lex r ])

instance Category Is where
  id = Refl
  {-# INLINE id #-}
  Refl . Refl = Refl
  {-# INLINE (.) #-}
