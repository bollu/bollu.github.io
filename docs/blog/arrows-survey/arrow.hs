{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Maybe
import Data.Semigroup


-- https://ac.els-cdn.com/S0167642399000234/1-s2.0-S0167642399000234-main.pdf
-- Implementation based on "generalizing monads to arrows"

class Arrow (a :: * -> * -> * ) where
  liftArr :: (i -> o) ->  a i o
  composeArr :: a b c -> a c d -> a b d
  -- provide a way to save an "x" value
  first :: a b c -> a (b, x) (c, x)

(>>>) :: Arrow a => a b c -> a c d -> a b d
(>>>) = composeArr

-- Identity arrow, useful to think about
-- living in the arrowd domain
idarr :: Arrow arr => arr x x
idarr = liftArr id


instance Arrow (->) where
  liftArr = id

  composeArr = flip (.)

  first :: (b -> c) -> ((b, x) -> (c, x))
  first f (b, x) = (f b, x)


newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Arrow (Kleisli m) where
  -- liftArr ab = Kleisli $ \a -> return (ab a)
  liftArr ab = Kleisli $ return . ab
  -- composeArr kamb kbmc = 
  --    Kleisli $ \a -> runKleisli kamb a >>= \b -> runKleisli kbmc b
  composeArr kamb kbmc = Kleisli $  runKleisli kamb >=> runKleisli kbmc

  first bmc = Kleisli $ \(b, x) -> do
    c <- runKleisli bmc b
    return (c, x)


liftM2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2' f ma mb = 
  do
    a <- ma
    b <- mb
    return $ f a b


arrswaptup :: Arrow arr => arr (b, c) (c, b)
arrswaptup = liftArr swap where
  swap (x, y) = (y, x)

second :: Arrow arr => arr b c -> arr (x, b) (x, c)
second btoc = arrswaptup  -- arr (x, b) (b, x)
  >>> first btoc  -- arr (b, x) (c, x)
  >>> arrswaptup -- arr (c, x) (x, c) 


(***) :: Arrow arr => arr p q -> arr x y -> arr (p, x) (q, y)
(***) pq xy = idarr -- arr (p, x) (p, x)
  >>> first pq
  >>> second xy

-- Arrow to duplicate a component
arrdup :: Arrow arr => arr b (b, b)
arrdup = liftArr (\b -> (b, b))

(&&&) :: Arrow arr => arr b c -> arr b d -> arr b (c, d)
(&&&) bc bd = arrdup -- arr b (b, b)
    >>> first bc
    >>> second bd


liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 f eb ec = (eb &&& ec) >>> liftArr (\(b, c) -> f b c)

-- # static and dynamic parsers

data EpsilonMatch = NoEpsilon | YesEpsilon

instance Semigroup EpsilonMatch where
  NoEpsilon <> NoEpsilon = NoEpsilon
  _ <> _ = YesEpsilon

instance Monoid EpsilonMatch where
  mempty = NoEpsilon
  mappend = (<>)

data StaticParser s = SP EpsilonMatch [s]

instance Semigroup (StaticParser s) where
  (SP ep1 ss1) <> (SP ep2 ss2) = SP (ep1 <> ep2) (ss1 <> ss2)


instance Monoid (StaticParser s) where
  mempty = SP mempty mempty
  mappend = (<>)

data DynamicParser s a = DP ([s] -> Maybe (a, [s]))
data Parser s a = P (StaticParser s) (DynamicParser s a)

symbol :: s -> Parser s s
symbol s = 
  P (SP NoEpsilon [s]) 
    (DP $ \(_:ss) -> Just (s, ss))

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (P s (DP dynamicparse)) = 
    P s (DP $ \ss -> do
                      (a, ss') <- dynamicparse ss
                      return (f a, ss'))

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = P (SP YesEpsilon []) 
               (DP $ \ss -> Just (a, ss))

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) (P sp1 (DP dp_a_to_b)) (P sp2 (DP dp_a)) = 
    P (sp1 <> sp2) 
      (DP $ \ss -> do
          (a_to_b, ss') <- dp_a_to_b ss
          (a, ss'') <- dp_a ss'
          return (a_to_b a, ss''))


-- matchdrop todrop xs = Just xs' -> removed prefix todrop from xs'
matchdrop :: Eq a => [a] -> [a] -> Maybe [a]
matchdrop [] as = Just as
matchdrop (p:ps) (a:as) = if p == a 
                          then matchdrop ps as
                          else Nothing

-- This is impossible, because we can't know the static
-- information of (a -> Parser s b)
-- instance Monad (Parser s) where
--     return :: a -> Parser s a
--     return = pure
--     
--     (>>=) ::  (Parser s a) -> (a -> Parser s b) -> Parser s b
--     (P (SP eps static) (DP dp_a)) >>= f = 
--       P (SP eps static)
--       (DP $ \ss -> do
--         (a, ss') <- dp_a ss
--         let (P (SP eps static') (DP dp_b)) = f a
--         -- pull out static' from ss'
--         -- Handle empty strings? for YesEpsilon
--         ss'' <- matchdrop static' ss'
--         dp_b ss''
--       )

data DynamicParser' s a b = DP' ((a, [s]) -> Maybe (b, [s]))
data Parser' s a b = P' (StaticParser s) (DynamicParser' s a b)

instance Arrow (Parser' s) where
  liftArr :: (a -> b) -> Parser' s a b
  liftArr f = P' mempty (DP' $ \(a, ss) -> Just (f a, ss))

  composeArr :: Parser' s a b -> Parser' s b c -> Parser' s a c
  composeArr (P' s1 (DP' ab)) (P' s2 (DP' bc)) =
      P' (s1 <> s2)  (DP' $ ab >=> bc)

  first :: Parser' s a b -> Parser' s (a, x) (b, x)
  first (P' s (DP' ab)) = 
    P' s 
      (DP' (\((a, x), ss) -> do
        (b, ss') <- ab (a, ss)
        return ((b, x), ss')))

--- Interpreter
type Id = String
data Expr = Var Id | Add Expr Expr | If Expr Expr Expr deriving(Show)
data Val = Num Int | Bl Bool deriving(Show)

vtrue :: Val
vtrue = Bl True

vfalse :: Val
vfalse = Bl False



type Env = [(Id, Val)]



-- Simple test expression, with sample environments
test1 :: Expr
test1 = If (Var "test") (Var "then") (Var "else")

test1_true_env :: Env
test1_true_env = [("test", vtrue), ("then", Num 42), ("else", Num (-42))]


test1_false_env :: Env
test1_false_env = [("test", vfalse), ("then", Num (-42)), ("else", Num (42))]



liftval2 :: (Int -> Int -> Int) -> Val -> Val -> Val
liftval2 f (Num i1) (Num i2) = Num (f i1 i2)
liftval2 f _ _ = error "non int values"

-- monadic evaluator
evalM :: Monad m => Expr -> Env -> m Val
evalM (Var id) env = return $ fromJust $ (lookup id env)
evalM (Add e1 e2) env = 
  liftM2 (liftval2 (+)) (evalM e1 env) (evalM e2 env)
evalM (If econd etrue efalse) env = do
  (Bl b) <- evalM econd env
  if b 
     then evalM etrue env 
     else evalM efalse env 

class Arrow arr => ArrowChoice arr where
  left :: arr i o -> arr (Either i x) (Either o x)


arrswapeither :: Arrow arr => arr (Either a b) (Either b a)
arrswapeither = liftArr (\x -> case x of
                                 Left a -> Right a
                                 Right b ->  Left b)

right :: ArrowChoice arr => arr i o -> arr (Either x i) (Either x o)
right io = arrswapeither >>> left io >>> arrswapeither

smashEither :: Either o o -> o
smashEither (Left l) = l
smashEither (Right r) = r

arrSmashEither :: Arrow arr => arr (Either o o) o
arrSmashEither = liftArr  smashEither

(|||) :: ArrowChoice arr => arr b o -> arr c o -> arr (Either b c) o
(|||) bo co = left bo -- arr (Either b c) (Either o c)
            >>> right co -- arr (Either b c) (Either o o)
            >>> arrSmashEither


instance ArrowChoice (->) where
  left :: (i -> o) -> (Either i x) -> (Either o x)
  left f (Left i) = Left (f i)
  left f (Right r) = Right r

-- arrow evaluator
evalA :: ArrowChoice arr => Expr -> arr Env Val
evalA (Var id) = liftArr $ \env -> fromJust $ (lookup id env)
evalA (Add e1 e2) = liftA2 (liftval2 (+)) (evalA e1) (evalA e2)
evalA (If econd etrue efalse) = 
  arrdup  -- arr Env (Env, Env)
  >>> first (evalA econd) -- arr Env (Val, Env)
  >>> (liftArr (\(Bl b, env) -> if b then Left env else Right env)) -- arr Env (Either Env Env)
  >>> ((evalA etrue) ||| (evalA efalse)) -- arr (Either Env Env) Val

-- (Zip Fusion with Hyperfunctions)
-- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.36.4961
-- Categories of Processes Enriched in Final Coalgebras
-- hyperfunctios
newtype H a b = Phi (H b a -> b)

(#@) :: H a b -> H b a -> b
(Phi f) #@ k = f k

konst :: a -> H b a
konst p = Phi (\k -> p)

(<<) :: (a -> b) -> H a b -> H a b
f << q = Phi (\k -> f (k #@ q))


lift :: (a->b) -> H a b
lift f = f << lift f


proj :: H a b -> a -> b
proj f a =f #@ (konst a)

self :: H a a
self = lift id

(#) :: H b c -> H a b -> H a c
f # g = Phi (\k -> f #@ (g # k))

run :: H a a -> a
run f = f #@ self

--- Machines
data SP a b = Put b (SP a b) | Get (a -> SP a b)
get = Get
put=Put

-- Causal commutative arrows
-- http://haskell.cs.yale.edu/wp-content/uploads/2012/06/FromJFP.pdf

main :: IO ()
main = undefined
