{-# LANGUAGE RankNTypes #-}

-- | <http://math.andrej.com/2009/10/12/constructive-gem-double-exponentials/ Constructive gem: double exponentials>

module Cantor
  ( _Natural
  ) where

import Control.Monad.Hyper
import Data.Profunctor
import Numeric.Natural

type Cantor = Natural -> Bool
type Functional = Cantor -> Bool

-- | <http://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/ Seemingly impossible functional programs>
find :: Functional -> Cantor
find p = branch x0 l0 (find (\r -> p (branch x0 l0 r))) where
  x0 = forsome (\l -> forsome (\r -> p (branch True l r)))
  l0 = find (\l -> forsome (\r -> p (branch x0 l r)))
  branch x _ _ 0 = x
  branch _ l r n
    | odd n = l ((n - 1) `div` 2)
    | otherwise = r ((n - 2) `div` 2)

forevery, forsome :: Functional -> Bool
forsome f = f (find f)
forevery f = not (forsome (not . f))

type Iso' s a = forall p f. (Profunctor p, Functor f) => p a (f a) -> p s (f s)

-- | Inductive Hyper functions from Bool to Bool are isomorphic to the natural numbers.
_Natural :: Iso' Natural (Hyper Bool Bool)
_Natural = dimap (ana enum) (fmap (cata denum)) where

  unpair :: Natural -> (Natural, Natural)
  unpair 0 = (0, 0)
  unpair n = (xr + 2*x, yr + 2*y) where
    (p, xr) = divMod n 2
    (q, yr) = divMod p 2
    (x, y) = unpair q

  enum :: Natural -> Functional
  enum 0 = const False
  enum 1 = const True
  enum n0 = fn' (n0-2) where
    fn' f a = compute 0 f where
      compute k 0 = not (a k)
      compute k 1 = a k
      compute k n | n' <- n - 2, q <- div n' 5 = case mod n' 5 of
        0 -> not (a k) && compute (k+1) q
        1 -> a k       && compute (k+1) q
        2 -> not (a k) || compute (k+1) q
        3 -> a k       || compute (k+1) q
        4 | (x, y) <- unpair q -> if a k
           then compute (k+1) y
           else compute (k+1) x

  pair :: Natural -> Natural -> Natural
  pair 0 0 = 0
  pair m n = mr + 2 * nr + 4 * pair mq nq where
    (mq, mr) = divMod m 2
    (nq, nr) = divMod n 2

  shift :: Bool -> Functional -> Functional
  shift b f = f . prepend b 
    
  prepend :: Bool -> Cantor -> Cantor 
  prepend bb _ 0 = bb 
  prepend _ a n = a (n-1)

  getConst :: Functional -> Maybe Bool
  getConst f
    | forevery (\a -> f a == b) = Just b
    | otherwise = Nothing
    where b = f (const False)

  denum :: Functional -> Natural
  denum f0 = case getConst f0 of
    Just False -> 0
    Just True -> 1
    Nothing -> 2 + go f0 where
      go f
        | a <- shift False f
        , b <- shift True f = case getConst a of
        Nothing -> case getConst b of
          Nothing -> 6 + 5 * pair (go a) (go b)
          Just False -> 2 + 5 * go a
          Just True -> 5 + 5 * go a
        Just False -> case getConst b of
          Nothing -> 3 + 5 * go b
          Just False -> error "impossible"
          Just True -> 1
        Just True -> case getConst b of
          Nothing -> 4 + 5 * go b
          Just False -> 0
          Just True -> error "impossible"
