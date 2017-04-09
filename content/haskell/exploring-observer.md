+++
Categories = []
Description = ""
Tags = []
date = "2017-02-20T00:09:15+05:30"
title = "exploring the observer pattern in Haskell"

+++


```haskell
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
import Control.Applicative
import Data.Traversable
import Data.Foldable

data Observer m e where
  MkObserver :: (e -> Maybe a) -> (s -> a -> m (Observer m e)) -> s -> Observer m e

simpleObserver :: Applicative m => (e -> Maybe a) -> (s -> a -> m s) -> s -> Observer m e
simpleObserver event handler s = o where
                o = MkObserver event handler' s
                handler' s a = fmap (\s' -> MkObserver event handler' s') (handler s a)

statelessObserver :: Applicative m => (e -> Maybe a) -> (a -> m()) -> Observer m e
statelessObserver event handler = o where
    o = simpleObserver event handler' ()
    handler' _ a = handler a <* pure o


data Dispatcher m e = Dispatcher {
  observers :: [Observer m e]
}


runEvent :: Applicative m => Dispatcher m e -> e -> m (Dispatcher m e) 
runEvent d e = Dispatcher <$> (for (observers d) run)  where
  run (observer @ (MkObserver handler action state)) = 
    case handler e of
        Just (a) ->  action state a
        Nothing -> pure observer


q_observer :: Observer IO String
q_observer = statelessObserver (\e -> if e == "q" then Just e else Nothing) (\_ -> print "q pressed")


w_observer :: Observer IO String
w_observer = simpleObserver (\e -> if e == "w" then Just e else Nothing)
      (\c _ -> do
          print ("w pressed: " ++ (show c))
          return $ (c + 1))
      0

mk_number_observer :: Int -> Observer IO String
mk_number_observer i = MkObserver 
                       (\e -> if e == show i then Just i else Nothing)
                       (\_ _ -> do
                                  print $ "number: " ++ show i
                                  return $ mk_number_observer ((i + 1) `mod` 10)
                        )
                        ()

 
runner :: Dispatcher IO String -> IO ()
runner d = do
  s <- getLine
  d' <- runEvent d s
  runner d'

main :: IO ()
main = do
  let dispatcher = Dispatcher [q_observer, q_observer, w_observer, mk_number_observer 0]
  runner dispatcher
```
