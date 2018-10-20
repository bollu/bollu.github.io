+++
title = 'Reading Kmett Machines'
date = 2018-10-20T17:51:12+05:30
draft = true
tags = ["tags"]
description = "Desc"

# For twitter cards, see https://github.com/mtn/cocoa-eh-hugo-theme/wiki/Twitter-cards
meta_img = "/images/image.jpg"

# For hacker news and lobsters builtin links, see github.com/mtn/cocoa-eh-hugo-theme/wiki/Social-Links
hacker_news_id = ""
lobsters_id = ""
+++

I wish to read the [machines](https://github.com/ekmett/machines) library, 
since I'm trying to investigate ways of cheaply embedding FPGA code generation
into Haskell --- So I'm reading about arrows, machines, and other ways
of embedding computations *within* haskell.

# Reading `Machines` Inside-out 
First, as usual, we run a `tree` on the repo:

```
.
├── benchmarks
│   └── Benchmarks.hs
├── CHANGELOG.markdown
├── config
├── examples
│   ├── Examples.hs
│   ├── LICENSE
│   └── machines-examples.cabal
├── LICENSE
├── machines.cabal
├── README.markdown
├── Setup.lhs
├── src
│   └── Data
│       ├── Machine
│       │   ├── Fanout.hs
│       │   ├── Group.hs
│       │   ├── Is.hs
│       │   ├── Lift.hs
│       │   ├── Mealy.hs
│       │   ├── MealyT.hs
│       │   ├── Moore.hs
│       │   ├── Pipe.hs
│       │   ├── Plan.hs
│       │   ├── Process.hs
│       │   ├── Runner.hs
│       │   ├── Source.hs
│       │   ├── Stack.hs
│       │   ├── Tee.hs
│       │   ├── Type.hs
│       │   └── Wye.hs
│       └── Machine.hs
├── tests
│   └── doctests.hs
└── Warning.hs
```

Let's look at `src/Data/Machine.hs`, `src/Data/Machine/{Type.hs, Plan.hs, Runner.hs, Process.hs}`
since my understanding of the lib is that we give it a `Plan` and it hands us
back a `Machine` that executes said `Plan`. Next, we can check
`src/Data/Machine/Mealy.hs`, because we understand what Mealy (and Moore)
machines are. After that, we can look at `examples/Examples.hs`, since looking
at it first would be too easy! It would also ould kill the point of this, to
read libraries inside-out.

## `src/Data/Machine.hs`:

```hs
module Data.Machine
  ( module Data.Machine.Is
  , module Data.Machine.Moore
  , module Data.Machine.Mealy
  , module Data.Machine.Plan
  , module Data.Machine.Process
  , module Data.Machine.Source
  , module Data.Machine.Tee
  , module Data.Machine.Type
  , module Data.Machine.Wye
  ) where

import Data.Machine.Is
import Data.Machine.Mealy
import Data.Machine.Moore
import Data.Machine.Plan
import Data.Machine.Process
import Data.Machine.Source
import Data.Machine.Tee
import Data.Machine.Type
import Data.Machine.Wye
```

So, this simply exports things. Let's look at the other files.

## `src/Data/Machine/Type.hs`
Reading `Type` is usually a good idea, since it's a grab-bag of things
the library builds on.

### `Step`

```hs
-- | This is the base functor for a 'Machine' or 'MachineT'.
--
-- Note: A 'Machine' is usually constructed from 'Plan', so it does not need to be CPS'd.
data Step k o r
  = Stop
  | Yield o r
  | forall t. Await (t -> r) (k t) r
```

OK, so from this, we get some context for what `Plan` is. We can see that a `Step`
can choose to:

- `Stop` stepping
- `Yield` an outpu value of `o`, and another value `r`.
- `Await`, in a way that is agnostic of the `t` (what even are inhabitants of
  `forall t. t -> r` other than `const r0`?), and do _something_ with it.

We're still not entirely sure whta an `r` is, but that can wait.


### `MachineT`
```hs
-- | A 'MachineT' reads from a number of inputs and may yield results before stopping
-- with monadic side-effects.
newtype MachineT m k o = MachineT { runMachineT :: m (Step k o (MachineT m k o)) }
```

Ah, interesting, so the `r` in `Step k o r` has the ability to return another
`MachineT`. So, `Yield`, and `Await` hold onto `MachineT`s.

We construct a `Machine` by universally quantifying over m:

### `Machine`
```hs
-- A 'Machine' can be used as a @'MachineT' m@ for any @'Monad' m@.
type Machine k o = forall m. Monad m => MachineT m k o
```

### `runT_`
There are a bunch of interesting instances in between, but let's zero
in on some of the the actually useful functions:

```
{-# INLINABLE runT_ #-}
runT_ :: Monad m => MachineT m k b -> m ()
runT_ m = runMachineT m >>= \v -> case v of
  Stop        -> return ()
  Yield _ k   -> runT_ k
  Await _ _ e -> runT_ e
```

recall that `runMachineT :: m (Step k b (MachineT m k b)) `. So, we now
have a `Step` that we need to interpret:

- If it's `Stop`, then we stop executing and return the unit effect
- If it's `Yield _ k :: Yield b (MachineT m k b)`, then we ignore the `o`, since we 
  don't care about the outputs right now, and just run the next machine.
- If it's an `Await _ _ e :: forall t. Await (t -> r) (k t) (MachineT m k b)`, we again run
  the machine in the third parameter (so we still don't really know what
  `Await` does, really)


### `runT`

`runT` is `runT_`'s older cousin, since it produces an `m [b]` out of the
`MachineT m k b`.

```hs
-- | Stop feeding input into model and extract an answer
{-# INLINABLE runT #-}
runT :: Monad m => MachineT m k b -> m [b]
runT (MachineT m) = m >>= \v -> case v of
  Stop        -> return []
  Yield o k   -> liftM (o:) (runT k)
  Await _ _ e -> runT e
```

Once again, we take the `m (Step k b (MachineT m k b))` in the machine,
and proceed to pattern match on it:

- If it's a `Stop`, we return the empty list
- If it's a `Yield o k :: Yield b (MachineT m k b)`, we run the
  `k` machine with a `runT k`, and cons `o` to it. So, `Yield`
  semantically provides an output value and the next computation to run.
- If it's an `Await _ _ e :: forall t. Await (t -> r) (k t) (MachineT m k b)`,
we do the same thing as `runT_`.

So far, we have understood `Stop` and `Yield`, but we still have no clue what
`Await` is. It's time to go looking for examples that tell us what it is.

### Looking for `Await`: `src/Data/Machine/Tee.hs`
Let's look for `Await` in a more principled way.

We have seen  that `MachineT m k o ~=(Step k o (MachineT m k o))`. The only
constructor of `Step` that uses `k` is 
` Step k o r = ... | forall t. Await (t -> r) (k t) r`. 

Hence, the semantics of `await` must be tied to the `k` parameter. It's only
logical for us to go look at uses of `Machine` that use an interesting `k`
type, which we find in `src/Data/Machines/Tee.hs`

```hs
```

The first non-trivial use of `Await` in a series of `Await`s is this one:

```hs
-- |
-- Connect different kinds of machines.
--
-- @'fit' 'id' = 'id'@
fit :: Monad m => (forall a. k a -> k' a) -> MachineT m k o -> MachineT m k' o
fit f (MachineT m) = MachineT (liftM f' m) where
  f' (Yield o k)     = Yield o (fit f k)
  f' Stop            = Stop
  f' (Await g kir h) = Await (fit f . g) (f kir) (fit f h)
```

So, let's think about what `let mach' = fit f mach` would do:

- We know that `f :: (foral a. k a -> k' a)` must be some kind of functorial
  function, since it's not allowed to assume anything about the shape of `a`
  when it converts `k a` to `k' a`.

- We give `mach :: MachineT m k o`, and receive a `mach' :: MachineT m k' o`.

## `src/Data/Machine/Plan.hs`:

### `PlanT`
```hs
newtype PlanT k o m a = PlanT
  { runPlanT :: forall r.
      (a -> m r) ->                                     -- Done a
      (o -> m r -> m r) ->                              -- Yield o (Plan k o a)
      (forall z. (z -> m r) -> k z -> m r -> m r) ->    -- forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
      m r ->                                            -- Fail
      m r
  }
```

So, first of all, there's a `k` that's not referenced anywhere.
That aside, it [looks like a continuation](TODO) thanks to the `forall r. ...`.
We need to understand what `Done`, `Yield`, `Await`, and `Fail` do, but
intutively, reading off the types (We can see how of I was at the end!)

-  `Done a` provides a way to give a *final* `a` value to the continuation `m r`.

- `Yield o (Plan k o a)` I assume provides access to a value `o` (the `output`?)

- `Await`, I'm not sure about, but it looks like a suspension that waits
for some input to construct another plan. Seems like a `Cont`.

- `Fail` provides a way to fail, and continue to the continuation `m r`.

### `Plan`
There's a similar `Plan`, which is `PlanT` universally quantified over 
the `m`, thereby not having the ability to know anything about the `m`:


```hs
-- @
-- data 'Plan' k o a
--   = Done a
--   | Yield o (Plan k o a)
--   | forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
--   | Fail
-- @
type Plan k o a = forall m. PlanT k o m a
```


OK, let's now view an example of a simple `Plan`:


### `yield`
```hs
-- | Output a result.
yield :: o -> Plan k o ()
yield o = PlanT (\kp ke _ _ -> ke o (kp ()))
```

Let's rename the fields to be a little more clear:

```hs
-- | Output a result.
yield :: o -> Plan k o ()
yield o = PlanT (\kdone kyield _ _ -> kyield o (kdone ()))
```
So, it returns a plan which yields an `o`, and ends with a `()`. So,
it calls the `kyield :: forall m. (o -> m r -> m r)` continuation with the
value `o`, and constructs an `m r` by calling `kdone :: forall m. () -> m r`.

### `stop`

```
instance Alternative (PlanT k o m) where
  empty = PlanT $ \_ _ _ kf -> kf
...
-- | @'stop' = 'empty'@
stop :: Plan k o a
stop = empty
```

`stop` simply returns the `kf :: m r`, as in, it fails immediately.

### `runPlan`


```hs
-- | Deconstruct a 'Plan' without reference to a 'Monad'.
runPlan :: PlanT k o Identity a
        -> (a -> r)
        -> (o -> r -> r)
        -> (forall z. (z -> r) -> k z -> r -> r)
        -> r
        -> r
runPlan m kp ke kr kf = runIdentity $ runPlanT m
  (Identity . kp)
  (\o (Identity r) -> Identity (ke o r))
  (\f k (Identity r) -> Identity (kr (runIdentity . f) k r))
  (Identity kf)
```

This is quite simple, it allows one to construct a `Plan` from `PlanT`
by filling in `Identity`. Nothing too great.


### Current thoughts

I still haven't seem much, and I don't have an intuition for `Plan` yet.
Let's now go check out `Moore`.

## `src/Data/Machine/Moore.hs`


```hs
-- | 'Moore' machines
data Moore a b = Moore b (a -> Moore a b)
```

This is straightforward.  It has an output `b`, and on an input `a`,
transitions to another `Moore a b`.

The file has a bunch of instances like `Functor, Applicative, Monad`, 
but there's nothing special to the library. However, there are a couple
interesting instances I want to 
**come back to: `ComonadApply, Distributive, Cosieve, Costrong, Closed`**.


