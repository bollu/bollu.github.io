{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Provide a notion of fanout wherein a single input is passed to
-- several consumers.
module Data.Machine.Fanout (fanout, fanoutSteps) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Machine
import           Data.Semigroup     (Semigroup (sconcat))
#if __GLASGOW_HASKELL__  < 710
import           Data.Monoid        (Monoid (..))
import           Data.Traversable   (traverse)
#endif

continue :: ([b] -> r) -> [(a -> b, b)] -> Step (Is a) o r
continue _ [] = Stop
continue f ws = Await (f . traverse fst ws) Refl (f $ map snd ws)

semigroupDlist :: Semigroup a => ([a] -> [a]) -> Maybe a
semigroupDlist f = case f [] of
  [] -> Nothing
  x:xs -> Just $ sconcat (x:|xs)

-- | Share inputs with each of a list of processes in lockstep. Any
-- values yielded by the processes are combined into a single yield
-- from the composite process.
fanout :: forall m a r. (Monad m, Semigroup r)
       => [ProcessT m a r] -> ProcessT m a r
fanout = MachineT . go id id
  where
    go :: ([(a -> ProcessT m a r, ProcessT m a r)]
       -> [(a -> ProcessT m a r, ProcessT m a r)])
       -> ([r] -> [r])
       -> [ProcessT m a r]
       -> m (Step (Is a) r (ProcessT m a r))
    go waiting acc [] = case waiting [] of
      ws -> return . maybe k (\x -> Yield x $ encased k) $ semigroupDlist acc
        where k = continue fanout ws
    go waiting acc (m:ms) = runMachineT m >>= \v -> case v of
      Stop           -> go waiting acc ms
      Yield x k      -> go waiting (acc . (x:)) (k:ms)
      Await f Refl k -> go (waiting . ((f, k):)) acc ms

-- | Share inputs with each of a list of processes in lockstep. If
-- none of the processes yields a value, the composite process will
-- itself yield 'mempty'. The idea is to provide a handle on steps
-- only executed for their side effects. For instance, if you want to
-- run a collection of 'ProcessT's that await but don't yield some
-- number of times, you can use 'fanOutSteps . map (fmap (const ()))'
-- followed by a 'taking' process.
fanoutSteps :: forall m a r. (Monad m, Monoid r)
            => [ProcessT m a r] -> ProcessT m a r
fanoutSteps = MachineT . go id id
  where
    go :: ([(a -> ProcessT m a r, ProcessT m a r)]
       -> [(a -> ProcessT m a r, ProcessT m a r)])
       -> ([r] -> [r])
       -> [ProcessT m a r]
       -> m (Step (Is a) r (ProcessT m a r))
    go waiting acc [] = case (waiting [], mconcat (acc [])) of
      (ws, xs) -> return . Yield xs $ encased (continue fanoutSteps ws)
    go waiting acc (m:ms) = runMachineT m >>= \v -> case v of
      Stop           -> go waiting acc ms
      Yield x k      -> go waiting (acc . (x:)) (k:ms)
      Await f Refl k -> go (waiting . ((f, k):)) acc ms
