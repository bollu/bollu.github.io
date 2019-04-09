{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Hyperfunctions as an explicit nu form, but using a representable functor
-- to describe the state space of the hyperfunction. This permits memoization
-- but doesn't require it.
--
-- If we start with a 'function with state' @(x -> a) -> x -> b@ we can view
-- it as either @(x -> a, x) -> b@ wich is a @Store x@ Cokleisli morphism or
-- as @φ :: x -> (x -> a) -> b@ which given @H a b x = (x -> a) -> b@ is a
-- @(H a b)@-coalgebra: @(x, φ)@ . Given that we can think of anamorphisms of
-- this 'function with state' as giving us a fixed point for @H a b@ and the
-- morphism to the final coalgebra @(Hyper a b, ana φ) is unique (by definition).
--
-- A representable functor @f@ is isomorphic to @(->) ('Rep' f)@. @((->) x)@
-- is an obvious selection for such a representable functor, so if we switch
-- out the functions from 'x' in the above, for a representable functor with
-- @x@ as its representation we get opportunities for memoization on the
-- internal 'state space' of our hyperfunctions.
--
-----------------------------------------------------------------------------
module Control.Monad.Hyper.Rep where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Fix
import Control.Monad.Zip
import Data.Distributive
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Rep
import Data.Profunctor
import Data.Profunctor.Unsafe
import Prelude hiding ((.),id)

-- | Represented Hyperfunctions
--
-- 'arr' is a faithful functor, so
--
-- @'arr' f ≡ 'arr' g@ implies @f ≡ g@

data Hyper a b where
  Hyper :: Representable g => g (g a -> b) -> Rep g -> Hyper a b

ana :: (x -> (x -> a) -> b) -> x -> Hyper a b
ana = Hyper

-- |
-- @
-- 'cata' phi ('push' f h) ≡ phi $ \\g -> f $ g ('cata' phi h)
-- @
cata :: (((y -> a) -> b) -> y) -> Hyper a b -> y
cata = cata'

-- | Memoizing catamorphism
cata' :: Representable f => ((f a -> b) -> Rep f) -> Hyper a b -> Rep f
cata' f (Hyper g x) = index h x where
  h = fmap (\k -> f (\fx -> k $ fmap (index fx) h)) g

instance Category Hyper where
  id = Hyper (Identity runIdentity) ()
  Hyper f x . Hyper g y = Hyper
    (Compose $ fmap (\phi -> fmap (\psi -> phi . fmap psi . getCompose) g) f)
    (x,y)

instance Arrow Hyper where
  arr f = Hyper (Identity (f .# runIdentity)) ()

  first (Hyper (f :: f (f a -> b)) x) = Hyper f' x where
    f' :: forall c. f (f (a,c) -> (b,c))
    f' = tabulate $ \i fac -> (index f i (fmap fst fac), snd (index fac i))

  second (Hyper (f :: f (f a -> b)) x) = Hyper f' x where
    f' :: forall c. f (f (c,a) -> (c,b))
    f' = tabulate $ \i fca -> (fst (index fca i), index f i (fmap snd fca))

  Hyper (f :: f (f a -> b)) x *** Hyper (g :: g (g c -> d)) y = Hyper h (x,y) where
    h :: Compose f g (Compose f g (a,c) -> (b, d))
    h = tabulate $ \(i,j) (Compose fgac) ->
      ( index f i (fmap (\gac -> fst (index gac j)) fgac)
      , index g j (fmap snd (index fgac i))
      )

  Hyper (f :: f (f a -> b)) x &&& Hyper (g :: g (g a -> c)) y = Hyper h (x,y) where
    h :: Compose f g (Compose f g a -> (b, c))
    h = tabulate $ \(i,j) (Compose fga) ->
      ( index f i (fmap (`index` j) fga)
      , index g j (index fga i)
      )

instance ArrowLoop Hyper where
  loop (Hyper f x) = Hyper (distribute f') x where
    f' fa = fmap fst $ fix $ \(r :: f (b,d)) ->
      distribute f $ tabulate $ \i -> (index fa i, snd $ index r i)

instance Functor (Hyper a) where
  fmap f (Hyper h x) = Hyper (fmap (f .) h) x

instance Applicative (Hyper a) where
  pure b = Hyper (Identity (const b)) ()
  p <* _ = p
  _ *> p = p
  Hyper (f :: f (f a -> b -> c)) x <*> Hyper (g :: g (g a -> b)) y = Hyper h (x,y) where
    h :: Compose f g (Compose f g a -> c)
    h = tabulate $ \(i,j) (Compose fga) ->
      index f i (fmap (`index` j) fga) (index g j (index fga i))

instance Monad (Hyper a) where
  return = pure
  m >>= f = cata (\g -> roll $ \k -> unroll (f (g k)) k) m

instance MonadZip (Hyper a) where
  munzip h = (fmap fst h, fmap snd h)
  mzipWith = liftA2

instance Profunctor Hyper where
  dimap f g (Hyper h x) = Hyper (fmap (\fa2b -> g . fa2b . fmap f) h) x

instance Strong Hyper where
  first' = first
  second' = second

instance Costrong Hyper where
  unfirst = loop

-- |
-- @
-- 'arr' f ≡ 'push' f ('arr' f)
-- 'invoke' ('push' f q) k ≡ f ('invoke' k q)
-- 'push' f p . 'push' g q ≡ 'push' (f . g) (p . q)
-- @
push :: (a -> b) -> Hyper a b -> Hyper a b
push f q = uninvoke $ \k -> f (invoke k q)

-- | Unroll a hyperfunction
unroll :: Hyper a b -> (Hyper a b -> a) -> b
unroll (Hyper (f :: f (f a -> b)) x) k = index f x (tabulate (k . Hyper f))

-- | Re-roll a hyperfunction using Lambek's lemma.
roll :: ((Hyper a b -> a) -> b) -> Hyper a b
roll = Hyper (mapH unroll) where
  -- mapH :: (x -> y) -> ((x -> a) -> b) -> (y -> a) -> b
  mapH xy xa2b ya = xa2b (ya . xy)


invoke :: Hyper a b -> Hyper b a -> b
invoke (Hyper (f :: f (f a -> b)) x) (Hyper (g :: g (g b -> a)) y) = index (index r x) y where
  -- tie a knot through state space
  r = fmap (\phi -> fmap (\psi -> phi (fmap psi r)) g) f

uninvoke :: (Hyper b a -> b) -> Hyper a b
uninvoke = Hyper (. roll)

-- |
-- @
-- 'run' f ≡ 'invoke' f 'id'
-- 'run' ('arr' f) ≡ 'fix' f
-- 'run' ('push' f q) ≡ f ('run' q)
-- 'run' ('push' f p . q) ≡ f ('run' (q . p)) = f ('invoke' q p)
-- @
run :: Hyper a a -> a
run (Hyper f x) = index r x where r = fmap ($ r) f


-- |
-- @
-- 'project' . 'arr' ≡ 'id'
-- 'project' h a ≡ 'invoke' h ('pure' a)
-- 'project' ('push' f q) ≡ f
-- @
project :: Hyper a b -> a -> b
project (Hyper f x) a = index f x (tabulate (const a))

-- |
-- <http://arxiv.org/pdf/1309.5135.pdf Under "nice" conditions>
--
-- @
-- 'fold' . 'build' ≡ 'id'
-- @
fold :: [a] -> (a -> b -> c) -> c -> Hyper b c
fold []     _ n = pure n
fold (x:xs) c n = push (c x) (fold xs c n)

build :: (forall b c. (a -> b -> c) -> c -> Hyper b c) -> [a]
build g = run (g (:) [])
