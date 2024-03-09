data DM pid m a where
  Ret :: a -> DM a
  Send :: pid -> m -> DM ()
  Recv :: (m -> DM a) -> DM a
