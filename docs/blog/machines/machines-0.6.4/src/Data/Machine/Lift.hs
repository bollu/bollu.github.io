-- | Utilities for working with machines that run in transformed monads,
-- inspired by @Pipes.Lift@.
module Data.Machine.Lift (execStateM, catchExcept, runReaderM) where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Machine.Type

-- | Given an initial state and a 'MachineT' that runs in @'StateT' s m@,
-- produce a 'MachineT' that runs in @m@.
execStateM :: Monad m => s -> MachineT (StateT s m) k o -> MachineT m k o
execStateM s m = MachineT $ do
  (stp, s') <- runStateT (runMachineT m) s
  case stp of
    Stop -> return Stop
    Yield o m' -> return $ Yield o (execStateM s' m')
    Await f k q -> return $ Await (execStateM s' . f) k (execStateM s' q)

-- | 'catchExcept' allows a broken machine to be replaced without stopping the
-- assembly line.
catchExcept :: Monad m
               => MachineT (ExceptT e m) k o
               -> (e -> MachineT (ExceptT e m) k o)
               -> MachineT (ExceptT e m) k o
catchExcept m c = MachineT $ do
  step <- runMachineT m `catchE` \e -> runMachineT (catchExcept (c e) c)
  case step of
    Stop -> return Stop
    Yield o m' -> return $ Yield o (catchExcept m' c)
    Await f k m' -> return $ Await (flip catchExcept c . f) k (catchExcept m' c)

-- | Given an environment and a 'MachineT' that runs in @'ReaderT' e m@,
-- produce a 'MachineT' that runs in @m@.
runReaderM :: Monad m => e -> MachineT (ReaderT e m) k o -> MachineT m k o
runReaderM e = fitM (flip runReaderT e)
