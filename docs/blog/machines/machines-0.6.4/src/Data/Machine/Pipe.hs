{-# LANGUAGE GADTs      #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Pipe
-- Copyright   :  (C) 2015 Yorick Laupa, Gabriel Gonzalez
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank-2 Types, GADTs
--
-- Allows bidirectional communication between two MachineT. Exposed the
-- same interface of Pipes library.
----------------------------------------------------------------------------
module Data.Machine.Pipe where

import Control.Monad

import Data.Void

import Data.Machine.Plan
import Data.Machine.Type

infixl 8 >~>
infixl 7 >+>
infixl 7 >>~
infixr 6 +>>

data Exchange a' a b' b c where
  Request :: a' -> Exchange a' a b' b a
  Respond :: b  -> Exchange a' a b' b b'

type Proxy a' a b' b m c = MachineT m (Exchange a' a b' b) c

-- | 'Effect's neither 'request' nor 'respond'
type Effect m r = Proxy Void () () Void m r

-- | @Client a' a@ sends requests of type @a'@ and receives responses of
--   type @a@. 'Client's only 'request' and never 'respond'.
type Client a' a m r = Proxy a' a () Void m r

-- | @Server b' b@ receives requests of type @b'@ and sends responses of type
--   @b@. 'Server's only 'respond' and never 'request'.
type Server b' b m r = Proxy Void () b' b m r

-- | Like 'Effect', but with a polymorphic type
type Effect' m r = forall x' x y' y . Proxy x' x y' y m r

-- | Like 'Server', but with a polymorphic type
type Server' b' b m r = forall x' x . Proxy x' x b' b m r

-- | Like 'Client', but with a polymorphic type
type Client' a' a m r = forall y' y . Proxy a' a y' y m r

-- | Send a value of type a' upstream and block waiting for a reply of type a.
--  'request' is the identity of the request category.
request :: a' -> PlanT (Exchange a' a y' y) o m a
request a = awaits (Request a)

-- | Send a value of type a downstream and block waiting for a reply of type a'
--  'respond' is the identity of the respond category.
respond :: a -> PlanT (Exchange x' x a' a) o m a'
respond a = awaits (Respond a)

-- | Forward responses followed by requests.
--   'push' is the identity of the push category.
push :: Monad m => a -> Proxy a' a a' a m r
push = construct . go
  where
    go = respond >=> request >=> go

-- | Compose two proxies blocked while 'request'ing data, creating a new proxy
--   blocked while 'request'ing data.
--   ('>~>') is the composition operator of the push category.
(>~>) :: Monad m
      => (_a -> Proxy a' a b' b m r)
      -> (b -> Proxy b' b c' c m r)
      -> _a -> Proxy a' a c' c m r
(fa >~> fb) a = fa a >>~ fb

-- | (p >>~ f) pairs each 'respond' in p with an 'request' in f.
(>>~) :: Monad m
      => Proxy a' a b' b m r
      -> (b -> Proxy b' b c' c m r)
      -> Proxy a' a c' c m r
pm >>~ fb = MachineT $ runMachineT pm >>= \p ->
  case p of
    Stop                    -> return Stop
    Yield r n               -> return $ Yield r (n >>~ fb)
    Await k (Request a') ff -> return $ Await (\a -> k a >>~ fb) (Request a') (ff >>~ fb)
    Await k (Respond b) _   -> runMachineT (k +>> fb b)

-- | Forward requests followed by responses.
--   'pull' is the identity of the pull category.
pull :: Monad m => a' -> Proxy a' a a' a m r
pull = construct . go
  where
    go = request >=> respond >=> go

-- | Compose two proxies blocked in the middle of 'respond'ing, creating a new
--   proxy blocked in the middle of 'respond'ing.
--   ('>+>') is the composition operator of the pull category.
(>+>) :: Monad m
      => (b' -> Proxy a' a b' b m r)
      -> (_c' -> Proxy b' b c' c m r)
      -> _c' -> Proxy a' a c' c m r
(fb' >+> fc') c' = fb' +>> fc' c'

-- | (f +>> p) pairs each 'request' in p with a 'respond' in f.
(+>>) :: Monad m
      => (b' -> Proxy a' a b' b m r)
      -> Proxy b' b c' c m r
      -> Proxy a' a c' c m r
fb' +>> pm = MachineT $ runMachineT pm >>= \p ->
  case p of
    Stop                   -> return Stop
    Yield r n              -> return $ Yield r (fb' +>> n)
    Await k (Request b') _ -> runMachineT (fb' b' >>~ k)
    Await k (Respond c) ff -> return $ Await (\c' -> fb' +>> k c') (Respond c) (fb' +>> ff)

-- | It is impossible for an `Exchange` to hold a `Void` value.
absurdExchange :: Exchange Void a b Void t -> c
absurdExchange (Request z) = absurd z
absurdExchange (Respond z) = absurd z

-- | Run a self-contained 'Effect', converting it back to the base monad.
runEffect :: Monad m => Effect m o -> m [o]
runEffect (MachineT m) = m >>= \v ->
  case v of
    Stop      -> return []
    Yield o n -> liftM (o:) (runEffect n)
    Await _ y _  -> absurdExchange y

-- | Like 'runEffect' but discarding any produced value.
runEffect_ :: Monad m => Effect m o -> m ()
runEffect_ (MachineT m) = m >>= \v ->
  case v of
    Stop      -> return ()
    Yield _ n -> runEffect_ n
    Await _ y _   -> absurdExchange y
