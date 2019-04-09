{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Process
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank 2 Types, GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Process
  (
  -- * Processes
    Process
  , ProcessT
  , Automaton(..)
  , AutomatonM(..)
  , process
  -- ** Common Processes
  , (<~), (~>)
  , echo
  , supply
  , prepended
  , filtered
  , dropping
  , taking
  , droppingWhile
  , takingWhile
  , buffered
  , flattened
  , fold
  , fold1
  , scan
  , scan1
  , scanMap
  , asParts
  , sinkPart_
  , autoM
  , final
  , finalOr
  , intersperse
  , largest
  , smallest
  , sequencing
  , mapping
  , traversing
  , reading
  , showing
  , strippingPrefix
  ) where

import Control.Category
import Control.Arrow (Kleisli(..))
import Control.Monad (liftM)
import Data.Foldable hiding (fold)
import Data.Machine.Is
import Data.Machine.Plan
import Data.Machine.Type
import Data.Monoid
import Data.Void
import Prelude
#if !(MIN_VERSION_base(4,8,0))
  hiding (id, (.), foldr)
#else
  hiding (id, (.))
#endif

-- $setup
-- >>> import Data.Machine.Source

infixr 9 <~
infixl 9 ~>

-------------------------------------------------------------------------------
-- Processes
-------------------------------------------------------------------------------

-- | A @'Process' a b@ is a stream transducer that can consume values of type @a@
-- from its input, and produce values of type @b@ for its output.
type Process a b = Machine (Is a) b

-- | A @'ProcessT' m a b@ is a stream transducer that can consume values of type @a@
-- from its input, and produce values of type @b@ and has side-effects in the
-- 'Monad' @m@.
type ProcessT m a b = MachineT m (Is a) b

-- | An 'Automaton' can be automatically lifted into a 'Process'
class Automaton k where
  auto :: k a b -> Process a b

instance Automaton (->) where
  auto = mapping

instance Automaton Is where
  auto Refl = echo

class AutomatonM x where
  autoT :: Monad m => x m a b -> ProcessT m a b

instance AutomatonM Kleisli where
  autoT (Kleisli k) = autoM k

-- | The trivial 'Process' that simply repeats each input it receives.
--
-- This can be constructed from a plan with
-- @
-- echo :: Process a a
-- echo = repeatedly $ do
--   i <- await
--   yield i
-- @
--
-- Examples:
--
-- >>> run $ echo <~ source [1..5]
-- [1,2,3,4,5]
--
echo :: Process a a
echo =
    loop
  where
    loop = encased (Await (\t -> encased (Yield t loop)) Refl stopped)
{-# INLINABLE echo #-}

-- | A 'Process' that prepends the elements of a 'Foldable' onto its input, then repeats its input from there.
prepended :: Foldable f => f a -> Process a a
prepended = before echo . traverse_ yield

-- | A 'Process' that only passes through inputs that match a predicate.
--
-- This can be constructed from a plan with
-- @
-- filtered :: (a -> Bool) -> Process a a
-- filtered p = repeatedly $ do
--   i <- await
--   when (p i) $ yield i
-- @
--
-- Examples:
--
-- >>> run $ filtered even <~ source [1..5]
-- [2,4]
--
filtered :: (a -> Bool) -> Process a a
filtered p =
    loop
  where
    loop = encased
         $ Await (\a -> if p a then encased (Yield a loop) else loop)
           Refl
           stopped
{-# INLINABLE filtered #-}

-- | A 'Process' that drops the first @n@, then repeats the rest.
--
-- This can be constructed from a plan with
-- @
-- dropping n = before echo $ replicateM_ n await
-- @
--
-- Examples:
--
-- >>> run $ dropping 3 <~ source [1..5]
-- [4,5]
--
dropping :: Int -> Process a a
dropping =
    loop
  where
    loop cnt
      | cnt <= 0
      = echo
      | otherwise
      = encased (Await (\_ -> loop (cnt - 1)) Refl stopped)
{-# INLINABLE dropping #-}

-- | A 'Process' that passes through the first @n@ elements from its input then stops
--
-- This can be constructed from a plan with
-- @
-- taking n = construct . replicateM_ n $ await >>= yield
-- @
--
-- Examples:
--
-- >>> run $ taking 3 <~ source [1..5]
-- [1,2,3]
--
taking :: Int -> Process a a
taking =
    loop
  where
    loop cnt
      | cnt <= 0
      = stopped
      | otherwise
      = encased (Await (\v -> encased $ Yield v (loop (cnt - 1))) Refl stopped)
{-# INLINABLE taking #-}

-- | A 'Process' that passes through elements until a predicate ceases to hold, then stops
--
-- This can be constructed from a plan with
-- @
-- takingWhile :: (a -> Bool) -> Process a a
-- takingWhile p = repeatedly $ await >>= \v -> if p v then yield v else stop
-- @
--
-- Examples:
--
-- >>> run $ takingWhile (< 3) <~ source [1..5]
-- [1,2]
--
takingWhile :: (a -> Bool) -> Process a a
takingWhile p =
    loop
  where
    loop = encased
         $ Await (\a -> if p a then encased (Yield a loop) else stopped)
           Refl
           stopped
{-# INLINABLE takingWhile #-}

-- | A 'Process' that drops elements while a predicate holds
--
-- This can be constructed from a plan with
-- @
-- droppingWhile :: (a -> Bool) -> Process a a
-- droppingWhile p = before echo loop where
--   loop = await >>= \v -> if p v then loop else yield v
-- @
--
-- Examples:
--
-- >>> run $ droppingWhile (< 3) <~ source [1..5]
-- [3,4,5]
--
droppingWhile :: (a -> Bool) -> Process a a
droppingWhile p =
    loop
  where
    loop = encased
         $ Await (\a -> if p a then loop else encased (Yield a echo))
           Refl
           stopped
{-# INLINABLE droppingWhile #-}

-- | Chunk up the input into `n` element lists.
--
-- Avoids returning empty lists and deals with the truncation of the final group.
--
-- An approximation of this can be constructed from a plan with
-- @
-- buffered :: Int -> Process a [a]
-- buffered = repeatedly . go [] where
--   go acc 0 = yield (reverse acc)
--   go acc n = do
--     i <- await <|> yield (reverse acc) *> stop
--     go (i:acc) $! n-1
-- @
--
-- Examples:
--
-- >>> run $ buffered 3 <~ source [1..6]
-- [[1,2,3],[4,5,6]]
--
-- >>> run $ buffered 3 <~ source [1..5]
-- [[1,2,3],[4,5]]
--
-- >>> run $ buffered 3 <~ source []
-- []
--
buffered :: Int -> Process a [a]
buffered n =
    begin
  where
    -- The buffer is empty, if we don't get anything
    -- then we shouldn't yield at all.
    begin     = encased
              $ Await (\v -> loop (v:) (n - 1))
                      Refl
                      stopped

    -- The buffer (a diff list) contains elements, and
    -- we're at the requisite number, yield the
    -- buffer and restart
    loop dl 0 = encased
              $ Yield (dl []) begin

    -- The buffer contains elements and we're not yet
    -- done, continue waiting, but if we don't receive
    -- anything, then yield what we have and stop.
    loop dl r = encased
              $ Await (\v -> loop (dl . (v:)) (r - 1))
                      Refl
                      (finish dl)

    -- All data has been retrieved, emit and stop.
    finish dl = encased
              $ Yield (dl []) stopped
{-# INLINABLE buffered #-}

-- | Build a new 'Machine' by adding a 'Process' to the output of an old 'Machine'
--
-- @
-- ('<~') :: 'Process' b c -> 'Process' a b -> 'Process' a c
-- ('<~') :: 'Process' c d -> 'Data.Machine.Tee.Tee' a b c -> 'Data.Machine.Tee.Tee' a b d
-- ('<~') :: 'Process' b c -> 'Machine' k b -> 'Machine' k c
-- @
(<~) :: Monad m => ProcessT m b c -> MachineT m k b -> MachineT m k c
mp <~ ma = MachineT $ runMachineT mp >>= \v -> case v of
  Stop          -> return Stop
  Yield o k     -> return $ Yield o (k <~ ma)
  Await f Refl ff -> runMachineT ma >>= \u -> case u of
    Stop          -> runMachineT $ ff <~ stopped
    Yield o k     -> runMachineT $ f o <~ k
    Await g kg fg -> return $ Await (\a -> encased v <~ g a) kg (encased v <~ fg)
{-# INLINABLE (<~) #-}

-- | Flipped ('<~').
(~>) :: Monad m => MachineT m k b -> ProcessT m b c -> MachineT m k c
ma ~> mp = mp <~ ma
{-# INLINABLE (~>) #-}

-- | Feed a 'Process' some input.
--
-- Examples:
--
-- >>> run $ supply [1,2,3] echo <~ source [4..6]
-- [1,2,3,4,5,6]
--
supply :: forall f m a b . (Foldable f, Monad m) => f a -> ProcessT m a b -> ProcessT m a b
supply = foldr go id
    where
      go :: a ->
            (ProcessT m a b -> ProcessT m a b) ->
            ProcessT m a b ->
            ProcessT m a b
      go x r m = MachineT $ do
         v <- runMachineT m
         case v of
           Stop -> return Stop
           Await f Refl _ -> runMachineT $ r (f x)
           Yield o k -> return $ Yield o (go x r k)
{-# INLINABLE supply #-}

-- |
-- Convert a machine into a process, with a little bit of help.
--
-- @
-- choose :: 'Data.Machine.Tee.T' a b x -> (a, b) -> x
-- choose t = case t of
--   'Data.Machine.Tee.L' -> 'fst'
--   'Data.Machine.Tee.R' -> 'snd'
--
-- 'process' choose :: 'Data.Machine.Tee.Tee' a b c -> 'Data.Machine.Process.Process' (a, b) c
-- 'process' choose :: 'Data.Machine.Tee.Tee' a b c -> 'Data.Machine.Process.Process' (a, b) c
-- 'process' ('const' 'id') :: 'Data.Machine.Process.Process' a b -> 'Data.Machine.Process.Process' a b
-- @
process :: Monad m => (forall a. k a -> i -> a) -> MachineT m k o -> ProcessT m i o
process f (MachineT m) = MachineT (liftM f' m) where
  f' (Yield o k)     = Yield o (process f k)
  f' Stop            = Stop
  f' (Await g kir h) = Await (process f . g . f kir) Refl (process f h)

-- |
-- Construct a 'Process' from a left-scanning operation.
--
-- Like 'fold', but yielding intermediate values.
--
-- It may be useful to consider this alternative signature
-- @
-- 'scan' :: (a -> b -> a) -> a -> Process b a
-- @
--
-- For stateful 'scan' use 'auto' with "Data.Machine.Mealy" machine.
-- This can be constructed from a plan with
-- @
-- scan :: Category k => (a -> b -> a) -> a -> Machine (k b) a
-- scan func seed = construct $ go seed where
--   go cur = do
--     yield cur
--     next <- await
--     go $! func cur next
-- @
--
-- Examples:
--
-- >>> run $ scan (+) 0 <~ source [1..5]
-- [0,1,3,6,10,15]
--
-- >>> run $ scan (\a _ -> a + 1) 0 <~ source [1..5]
-- [0,1,2,3,4,5]
--
scan :: Category k => (a -> b -> a) -> a -> Machine (k b) a
scan func seed =
  let step t = t `seq` encased
             $ Yield t
             $ encased
             $ Await (step . func t)
                     id
                     stopped
  in  step seed
{-# INLINABLE scan #-}

-- |
-- 'scan1' is a variant of 'scan' that has no starting value argument
--
-- This can be constructed from a plan with
-- @
-- scan1 :: Category k => (a -> a -> a) -> Machine (k a) a
-- scan1 func = construct $ await >>= go where
--   go cur = do
--     yield cur
--     next <- await
--     go $! func cur next
-- @
--
-- Examples:
--
-- >>> run $ scan1 (+) <~ source [1..5]
-- [1,3,6,10,15]
--
scan1 :: Category k => (a -> a -> a) -> Machine (k a) a
scan1 func =
  let step t = t `seq` encased
             $ Yield t
             $ encased
             $ Await (step . func t)
                     id
                     stopped
  in  encased $ Await step id stopped
{-# INLINABLE scan1 #-}

-- |
-- Like 'scan' only uses supplied function to map and uses Monoid for
-- associative operation
--
-- Examples:
--
-- >>> run $ mapping getSum <~ scanMap Sum <~ source [1..5]
-- [0,1,3,6,10,15]
--
scanMap :: (Category k, Monoid b) => (a -> b) -> Machine (k a) b
scanMap f = scan (\b a -> mappend b (f a)) mempty
{-# INLINABLE scanMap #-}

-- |
-- Construct a 'Process' from a left-folding operation.
--
-- Like 'scan', but only yielding the final value.
--
-- It may be useful to consider this alternative signature
-- @
-- 'fold' :: (a -> b -> a) -> a -> Process b a
-- @
--
-- This can be constructed from a plan with
-- @
-- fold :: Category k => (a -> b -> a) -> a -> Machine (k b) a
-- fold func seed = construct $ go seed where
--   go cur = do
--     next <- await <|> yield cur *> stop
--     go $! func cur next
-- @
--
-- Examples:
--
-- >>> run $ fold (+) 0 <~ source [1..5]
-- [15]
--
-- >>> run $ fold (\a _ -> a + 1) 0 <~ source [1..5]
-- [5]
--
fold :: Category k => (a -> b -> a) -> a -> Machine (k b) a
fold func =
  let step t = t `seq` encased
             $ Await (step . func t)
                     id
                     (encased $ Yield t stopped)
  in  step
{-# INLINABLE fold #-}

-- |
-- 'fold1' is a variant of 'fold' that has no starting value argument
--
-- This can be constructed from a plan with
-- @
-- fold1 :: Category k => (a -> a -> a) -> Machine (k a) a
-- fold1 func = construct $ await >>= go where
--   go cur = do
--     next <- await <|> yield cur *> stop
--     go $! func cur next
-- @
--
-- Examples:
--
-- >>> run $ fold1 (+) <~ source [1..5]
-- [15]
--
fold1 :: Category k => (a -> a -> a) -> Machine (k a) a
fold1 func =
  let step t = t `seq` encased
             $ Await (step . func t)
                     id
                     (encased $ Yield t stopped)
  in  encased $ Await step id stopped
{-# INLINABLE fold1 #-}

-- | Break each input into pieces that are fed downstream
-- individually.
--
-- This can be constructed from a plan with
-- @
-- asParts :: Foldable f => Process (f a) a
-- asParts = repeatedly $ await >>= traverse_ yield
-- @
--
-- Examples:
--
-- >>> run $ asParts <~ source [[1..3],[4..6]]
-- [1,2,3,4,5,6]
--
asParts :: Foldable f => Process (f a) a
asParts =
  let step = encased
           $ Await (foldr (\b s -> encased (Yield b s)) step)
                   id
                   stopped
  in  step
{-# INLINABLE asParts #-}

-- | Break each input into pieces that are fed downstream
-- individually.
--
-- Alias for @asParts@
--
flattened :: Foldable f => Process (f a) a
flattened = asParts
{-# INLINABLE flattened #-}

-- | @sinkPart_ toParts sink@ creates a process that uses the
-- @toParts@ function to break input into a tuple of @(passAlong,
-- sinkPart)@ for which the second projection is given to the supplied
-- @sink@ 'ProcessT' (that produces no output) while the first
-- projection is passed down the pipeline.
sinkPart_ :: Monad m => (a -> (b,c)) -> ProcessT m c Void -> ProcessT m a b
sinkPart_ p = go
  where go m = MachineT $ runMachineT m >>= \v -> case v of
          Stop -> return Stop
          Yield o _ -> absurd o
          Await f Refl ff -> return $
            Await (\x -> let (keep,sink) = p x
                         in encased . Yield keep $ go (f sink))
                  Refl
                  (go ff)

-- | Apply a monadic function to each element of a 'ProcessT'.
--
-- This can be constructed from a plan with
-- @
-- autoM :: Monad m => (a -> m b) -> ProcessT m a b
-- autoM :: (Category k, Monad m) => (a -> m b) -> MachineT m (k a) b
-- autoM f = repeatedly $ await >>= lift . f >>= yield
-- @
--
-- Examples:
--
-- >>> runT $ autoM Left <~ source [3, 4]
-- Left 3
--
-- >>> runT $ autoM Right <~ source [3, 4]
-- Right [3,4]
--
autoM :: (Category k, Monad m) => (a -> m b) -> MachineT m (k a) b
autoM f =
    loop
  where
    loop = encased (Await (\t -> MachineT (flip Yield loop `liftM` f t)) id stopped)
{-# INLINABLE autoM #-}

-- |
-- Skip all but the final element of the input
--
-- This can be constructed from a plan with
-- @
-- 'final' :: 'Process' a a
-- final :: Category k => Machine (k a) a
-- final = construct $ await >>= go where
--   go prev = do
--     next <- await <|> yield prev *> stop
--     go next
-- @
--
-- Examples:
--
-- >>> runT $ final <~ source [1..10]
-- [10]
-- >>> runT $ final <~ source []
-- []
--
final :: Category k => Machine (k a) a
final =
  let step x = encased (Await step id (emit x))
      emit x = encased (Yield x stopped)
  in encased $ Await step id stopped
{-# INLINABLE final #-}

-- |
-- Skip all but the final element of the input.
-- If the input is empty, the default value is emitted
--
-- This can be constructed from a plan with
-- @
-- 'finalOr' :: a -> 'Process' a a
-- finalOr :: Category k => a -> Machine (k a) a
-- finalOr = construct . go where
--   go prev = do
--     next <- await <|> yield prev *> stop
--     go next
-- @
--
-- Examples:
--
-- >>> runT $ finalOr (-1) <~ source [1..10]
-- [10]
-- >>> runT $ finalOr (-1) <~ source []
-- [-1]
--
finalOr :: Category k => a -> Machine (k a) a
finalOr =
  let step x = encased (Await step id (emit x))
      emit x = encased (Yield x stopped)
  in step
{-# INLINABLE finalOr #-}

-- |
-- Intersperse an element between the elements of the input
--
-- @
-- 'intersperse' :: a -> 'Process' a a
-- @
intersperse :: Category k => a -> Machine (k a) a
intersperse sep = construct $ await >>= go where
  go cur = do
    yield cur
    next <- await
    yield sep
    go next

-- |
-- Return the maximum value from the input
largest :: (Category k, Ord a) => Machine (k a) a
largest = fold1 max
{-# INLINABLE largest #-}

-- |
-- Return the minimum value from the input
smallest :: (Category k, Ord a) => Machine (k a) a
smallest = fold1 min
{-# INLINABLE smallest #-}

-- |
-- Convert a stream of actions to a stream of values
--
-- This can be constructed from a plan with
-- @
-- sequencing :: Monad m => (a -> m b) -> ProcessT m a b
-- sequencing :: (Category k, Monad m) => MachineT m (k (m a)) a
-- sequencing = repeatedly $ do
--   ma <- await
--   a  <- lift ma
--   yield a
-- @
--
-- Examples:
--
-- >>> runT $ sequencing <~ source [Just 3, Nothing]
-- Nothing
--
-- >>> runT $ sequencing <~ source [Just 3, Just 4]
-- Just [3,4]
--
sequencing :: (Category k, Monad m) => MachineT m (k (m a)) a
sequencing = autoM id
{-# INLINABLE sequencing #-}

-- |
-- Apply a function to all values coming from the input
--
-- This can be constructed from a plan with
-- @
-- mapping :: Category k => (a -> b) -> Machine (k a) b
-- mapping f = repeatedly $ await >>= yield . f
-- @
--
-- Examples:
--
-- >>> runT $ mapping (*2) <~ source [1..3]
-- [2,4,6]
--
mapping :: Category k => (a -> b) -> Machine (k a) b
mapping f =
    loop
  where
    loop = encased (Await (\t -> encased (Yield (f t) loop)) id stopped)
{-# INLINABLE mapping #-}

-- |
-- Apply an effectful to all values coming from the input.
--
-- Alias to 'autoM'.
traversing :: (Category k, Monad m) => (a -> m b) -> MachineT m (k a) b
traversing = autoM

-- |
-- Parse 'Read'able values, only emitting the value if the parse succceeds.
-- This 'Machine' stops at first parsing error
reading :: (Category k, Read a) => Machine (k String) a
reading = repeatedly $ do
  s <- await
  case reads s of
    [(a, "")] -> yield a
    _         -> stop

-- |
-- Convert 'Show'able values to 'String's
showing :: (Category k, Show a) => Machine (k a) String
showing = mapping show
{-# INLINABLE showing #-}

-- |
-- 'strippingPrefix' @mp mb@ Drops the given prefix from @mp@. It stops if @mb@
-- did not start with the prefix given, or continues streaming after the
-- prefix, if @mb@ did.
strippingPrefix :: (Eq b, Monad m)
                => MachineT m (k a) b
                -> MachineT m (k a) b
                -> MachineT m (k a) b
strippingPrefix mp mb = MachineT $ runMachineT mp >>= \v -> case v of
  Stop          -> runMachineT mb
  Yield b k     -> verify b k mb
  Await f ki ff ->
    return $ Await (\a -> strippingPrefix (f a) mb) ki (strippingPrefix ff mb)
  where
    verify b nxt cur = runMachineT cur >>= \u -> case u of
      Stop -> return Stop
      Yield b' nxt'
        | b == b'   -> runMachineT $ strippingPrefix nxt nxt'
        | otherwise -> return Stop
      Await f ki ff ->
        return $ Await (MachineT . verify b nxt . f)
                    ki (MachineT $ verify b nxt ff)
