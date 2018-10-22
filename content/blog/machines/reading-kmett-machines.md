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

There's not much to see, so I'll visit `src/Data/Machine/Plan.hs`, and see if that
can lead me to `Machine`.

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

So, first of all, there's a `k :: * -> *` (higher kinded) which is only
referenced in `Await`.

That aside, it [looks like a continuation](TODO) thanks to the `forall r. ...`.
We need to understand what `Done`, `Yield`, `Await`, and `Fail` do, but
intutively, reading off the types (We can see how of I was at the end!)

-  `Done a` provides a way to give a *final* `a` value to the continuation `m r`.

- `Yield o (Plan k o a)` I assume provides access to a value `o` (the `output`?)

- `Await` seems to contain both a suspension of a `Plan`, some auxiliary data,
and a Plan. The fully thing is that I can't see a reason for `forall z. (z -> m r)`
(what are its inhabitants, except for for `const (mr)`?) We also have a 
`k z` value (where once again, we can assume nothing about `z`, so the only thing
we can "learn" is the shape of `k`), and a `Plan k o a`.  I assume that the
final `Plan k o a` is used by default, while the `k z` will be used for some
magic trick I'm frankly
excited to see.

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

**PLAN IS A MONAD SO WE CAN USE DO-NOTATION TO BUILD IT.**
**MACHINES ARE REIFIED FROM PLAN**

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

### `Monad` instance of `PlanT`:

The entire reason for `PlanT` to exist (as was clarified to me by `daevan`
on `#haskell`) is to provide a monadic interface to build `Machine`s, which
can then be _reified_ into a `Machine`.

So, let's jump in to take a look at the `Monad` instance of `PlanT`:

```hs
--src/Data/Machine/Plan.hs
instance Applicative (PlanT k o m) where
  pure a = PlanT (\kp _ _ _ -> kp a)
  ...
...
instance Monad (PlanT k o m) where
  return = pure
  PlanT m >>= f = PlanT (\kp ke kr kf -> m (\a -> runPlanT (f a) kp ke kr kf) ke kr kf)
  fail = Fail.fail
```

So, the monad instance is such that it calls the original `PlanT`'s continuation
`m`, with the `done` continuation being "run the new pipeline, modified with `f`,
and keeps the other continuations the same.

`return` / `pure` simply call `Done` immediately.



### Current thoughts

I still haven't seem much, and I don't have an intuition for `Plan` yet.
Let's go look at `src/Data/Machine/Type.hs` to see if we can find
our elusive `Machine` type.

## `src/Data/Machine/Type.hs`
Reading `Type` is usually a good idea, since it's a grab-bag of things
the library builds on.

### `Step`

```hs
-- src/Data/Machine/Type.hs
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
-- src/Data/Machine/Type.hs
-- | A 'MachineT' reads from a number of inputs and may yield results before stopping
-- with monadic side-effects.
newtype MachineT m k o = MachineT { runMachineT :: m (Step k o (MachineT m k o)) }
```

Ah, interesting, so the `r` in `Step k o r` has the ability to return another
`MachineT`. So, `Yield`, and `Await` hold onto `MachineT`s.

We construct a `Machine` by universally quantifying over m:

### `Machine`
```hs
-- src/Data/Machine/Type.hs
-- A 'Machine' can be used as a @'MachineT' m@ for any @'Monad' m@.
type Machine k o = forall m. Monad m => MachineT m k o
```

### `runT_`
There are a bunch of interesting instances in between, but let's zero
in on some of the the actually useful functions:

```
{-# INLINABLE runT_ #-}
-- src/Data/Machine/Type.hs
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
-- src/Data/Machine/Type.hs
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

#### `TeeT`
```hs
-- src/Data/Machine/Tee.hs
-- | The input descriptor for a 'Tee' or 'TeeT'
data T a b c where
  L :: T a b a
  R :: T a b b

-- | A 'Machine' that can read from two input stream 
--   in a deterministic manner with monadic side-effects.
type TeeT m a b c = MachineT m (T a b) c
```

OK, so far, nothing enlightening. Next, we see this comment:

#### `teeT`
```hs
-- src/Data/Machine/Tee.hs
-- | `teeT mt ma mb` Use a `Tee` to interleave or combine the outputs of `ma`
--   and `mb`.
--
--   The resulting machine will draw from a single source.
--
-- Examples:
--
-- >>> import Data.Machine.Source
-- >>> run $ teeT zipping echo echo <~ source [1..5]
-- [(1,2),(3,4)]
--
teeT :: Monad m => TeeT m a b c -> MachineT m k a -> MachineT m k b -> MachineT m k c
...
```

This is interesing. By calling `teeT`, we are able to produce a machine that
combines output types `a` and `b` into a type `c`. Since for a `TeeT`, `k`
parameter of `MachineT m k o` is `T a b`, the key to the combination should lie
in the structure of `TeeT`. So we should understand how to construct a `TeeT`.
Hunting for this, we find:

#### `Tee.zipWith`

```hs
-- src/Data/Machine/Tee.hs
-- | Zip together two inputs, then apply the given function,
--   halting as soon as either input is exhausted.
--   This implementation reads from the left, then the right
zipWith :: (a -> b -> c) -> Tee a b c
zipWith f = repeatedly $ do
  a <- awaits L
  b <- awaits R
  yield (f a b)
```

Cool, we now need to find `repeatedly` and `awaits`, and we're golden :)
Guessing though, it looks like `repeatedly` simply loops the given computation,
and `awaits` tries to wait for a value that matches the **data constructor `L :: T a b a`
and `R :: T a b b`**.

How in the world do you match on a **data constructor**? I guess we
grep and look for the implementation of `awaits`.


#### `awaits`

Grepping (well, [silver searching](https://github.com/ggreer/the_silver_searcher))
```
$ ag "awaits ::"                                                                                                          148 
src/Data/Machine/Plan.hs
192:awaits :: k i -> Plan k o i
```


Let's run the `awaits L`, since I want to see what the types look like:
```
-- stack ghci
*Examples Data.Machine> :info awaits T
awaits :: k i -> Plan k o i
type role T nominal nominal nominal
data T a b c where
  L :: T a b a
  ...
*Examples Data.Machine> :t (awaits L)
(awaits L) :: PlanT (T i b) o m i
```

I was stumped by the type of `awaits L`, till `RyanGLScott` explained it
to me on the IRC channel (`#haskell` on freenode):

```
-- working something out on paper
awaits :: k i -> Plan k o i
L :: T a b a ~ T i b i ~ (T i b) i

k i ~ (T i b) i
k ~ (T i b)
i ~ i

Plan k o i ~ Plan (T i b) o i ~ PlanT (T i b) o m i
```

I find the type utterly unenlightening, since I do not yet know what the `k`
parameter of a `PlanT` does anyway. It looks like the `Done` value can be an
`i`, and the continuation is a `T i b`? Let's understand the implementation:

```hs
-- src/Data/Machine/Plan.hs
awaits :: k i -> Plan k o i
awaits h = PlanT $ \kp _ kr -> kr kp h
```

Let's eta-expand and rename terse variable names to better understand the type:
```hs
-- temporary code
awaits' :: k i -> Plan k o i
awaits' h = PlanT $ f h where
  f :: k i ->  (forall r. (i -> r) -> 
                      (o -> r -> r) -> 
                      (forall z. (z -> r) -> k z -> r -> r) -> 
                       r -> r)
  f h' done yield await fail = await done h' fail
```

Note the types here:
```hs
h' :: k i 
done :: forall r. (i -> r)
yield :: (o -> r -> r)
await :: (forall z. (z -> r) -> k z -> r -> r) ~(setting z=i)~
                     (i -> r) -> k i -> r -> r
fail :: r
```

So, we specialize the `await` with `z ~ i`, allowing us to feed
the `await` the `h :: k i`, which also takes the `done :: i -> r`
and `fail :: r` continuations, to return the final value.

I don't really understand it, but it seems to to be used to "plug a `k i`"
temporary into the computation of `r`.

Looking at the un-CPSd form,
```h
forall z. Await 
            (z -> Plan k o a) -- done value
            (k z) -- temporary that is plugged in by awaits
            (Plan k o a) -- fail value
```

It's natural to wonder, "do we need the `forall z.` freedom in `Await`?
Removing the `forall z.` gives us errors:

```
../machines-0.6.4/src/Data/Machine/Plan.hs:96:36: error:
    • Couldn't match type ‘a’ with ‘b’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          fmap :: forall a b. (a -> b) -> PlanT k o m a -> PlanT k o m b
        at ../machines-0.6.4/src/Data/Machine/Plan.hs:96:3-6
      ‘b’ is a rigid type variable bound by
        the type signature for:
          fmap :: forall a b. (a -> b) -> PlanT k o m a -> PlanT k o m b
        at ../machines-0.6.4/src/Data/Machine/Plan.hs:96:3-6
      Expected type: (o -> m r -> m r)
                     -> ((b -> m r) -> k b -> m r -> m r) -> m r -> m r
        Actual type: (o -> m r -> m r)
                     -> ((a -> m r) -> k a -> m r -> m r) -> m r -> m r
    • In the expression: m (k . f)
      In the second argument of ‘($)’, namely ‘\ k -> m (k . f)’
      In the expression: PlanT $ \ k -> m (k . f)
    • Relevant bindings include
        k :: b -> m r
          (bound at ../machines-0.6.4/src/Data/Machine/Plan.hs:96:31)
        m :: forall r.
             (a -> m r)
             -> (o -> m r -> m r)
             -> ((a -> m r) -> k a -> m r -> m r)
             -> m r
             -> m r
          (bound at ../machines-0.6.4/src/Data/Machine/Plan.hs:96:17)
        f :: a -> b
          (bound at ../machines-0.6.4/src/Data/Machine/Plan.hs:96:8)
        fmap :: (a -> b) -> PlanT k o m a -> PlanT k o m b
          (bound at ../machines-0.6.4/src/Data/Machine/Plan.hs:96:3)
   |
96 |   fmap f (PlanT m) = PlanT $ \k -> m (k . f)
   |                                    ^^^^^^^^^
```

Interesting, so it looks like we need the freedom in `Await` to be able to
define a `Functor` for `PlanT`! OK, let's think about this now --- if we want a `Functor`
instance for `PlanT`, why do we need this extra freedom?


```hs
-- **WRONG PlanT**, without (forall z.) freedom in Await:

instance Functor (PlanT k o m) where
  -- fmap :: (a -> b) -> PlanT k o m a -> PlanT k o m b
  -- fmap :: (a -> b) -> 
      (forall r. (a -> m r) ->                                     -- Done a
      (o -> m r -> m r) ->                              -- Yield o (Plan k o a)
      ((a -> m r) -> k a -> m r -> m r) ->    -- forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
      m r ->                                            -- Fail
      m r)) -> 
  fmap f (PlanT m) = PlanT $ \k -> m (k . f)

f :: a -> b
m :: (forall r. (a -> m r) ->                                     -- Done a
      (o -> m r -> m r) ->                              -- Yield o (Plan k o a)
      ((a -> m r) -> k a -> m r -> m r) ->    -- forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
      m r ->                                            -- Fail
      m r))

k :: forall s. (b -> m s)
```

Intuitively, we lack the freedom to "reach into `await`" and convert the `a -> m r`
to a `b -> m r` (that would be `Contravariant`, not `Functor/Covariant`). So,
we need the flexibility of a `forall z.` to be able to define a `Functor` for the
`Await` part of `Plan`.

#### `repeatedly`
Grepping for `repeatedly`:

```
╰─$ ag "repeatedly ::"                                                                                                      148 ↵
src/Data/Machine/Type.hs
226:repeatedly :: Monad m => PlanT k o m a -> MachineT m k o
```

Reading the source,

```hs
-- src/Data/Machine/Type.hs
-- @'repeatedly' m = 'construct' ('Control.Monad.forever' m)@
repeatedly :: Monad m => PlanT k o m a -> MachineT m k o
repeatedly m = r where
  r = MachineT $ runPlanT m
    (const (runMachineT r))
    (\o k -> return (Yield o (MachineT k)))
    (\f k g -> return (Await (MachineT #. f) k (MachineT g)))
    (return Stop)
```

#### Side track-`#.`

We see the `#.` in the definition of `repeatedly`, so let's
first understand that:

```
(#.) :: forall a b c q. Coercible c b => q b c -> p a b -> p a c 
```

[The documentation says:](http://hackage.haskell.org/package/profunctors-5.3/docs/Data-Profunctor-Unsafe.html)


Strictly map the second argument argument covariantly with a function that is
assumed operationally to be a cast, such as a newtype constructor.

Cool, so given `b -q-> c`, a way to coerce `b` into `c`, it lets us
convert  `a -p-> b` to `a -p-> c`. Seems straightfoward:

```
(coercion #. fn) === a -fn-> b -coercion-> c
```

At this point, `Await` is beginning to look weirder and weirder, why does
it need a way to `coerce` data?



#### Getting back to `repeatedly`:

```hs
-- src/Data/Machine/Type.hs
-- @'repeatedly' m = 'construct' ('Control.Monad.forever' m)@
repeatedly :: Monad m => PlanT k o m a -> MachineT m k o
repeatedly m = r where
  r = MachineT $ runPlanT m
    (const (runMachineT r))
    (\o k -> return (Yield o (MachineT k)))
    (\f k g -> return (Await (MachineT #. f) k (MachineT g)))
    (return Stop)
```

so, given a `PlanT`, it constructs a `MachineT` (nice, we finally start
seeing the interplay between `Plan` and `Machine`!).

Let's unpack this, first let's remember the types:

```hs
newtype PlanT k o m a = PlanT
  { runPlanT :: forall r.
      (a -> m r) ->                                     -- Done a
      (o -> m r -> m r) ->                              -- Yield o (Plan k o a)
      (forall z. (z -> m r) -> k z -> m r -> m r) ->    -- forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
      m r ->                                            -- Fail
      m r
  }

runMachineT :: m (Step k o (MachineT m k o))

data Step k o r
  = Stop
  | Yield o r
  | forall t. Await (t -> r) (k t) r
```

So, `repeatedly` constructs a `MachineT m k o`, which needs a `m (Step k o (MachineT m k o)`.

- for the `(a -> m r) ~ Done a` parameter of `runPlanT`, it uses
    `(const (runMachineT r))`, which ignores the `a`, and produces  `m (Step k
    o (MachineT m k o)`. That is, re-runs the machine `r`, and provides the
    next step to `runMachineT`.

- for the `(o -> m r -> m r) ~ Yield o (Plan k o a)`, it uses
    `(\o k -> return (Yield o (MachineT k)))`. That is, it simply constructs
    the correct `Yield` step of the machine.

- for the `(forall z. (z -> m r) -> k z -> m r -> m r) ~ forall z. Await (z -> Plan k o a) (k z) (Plan k o a)`,
    it uses `(\f k g -> return (Await (MachineT #. f) k (MachineT g)))`.




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


