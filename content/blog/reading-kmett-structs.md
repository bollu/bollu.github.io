+++
title = "Reading-Kmett-Structs"
date = "2018-10-04T19:53:32+05:30"
draft = true

+++

I'm examining Edward Kmett's [structs](TODO) package, at commit
[`344da9641274cb774`](TODO). I decided to write up fun bits of the code
I found as I spelunked on the codebase, as well as motivate why this
library exists (as far as I can tell, anyway)

# Introduction

I'm going in blind, so let's first read the README, and then
jump directly intowhat's exported by this library!


## README

The `README` says:

```
This package explores strict mutable data structures in Haskell.
In particular, pointer-based data structures are effectively 'half price' due
to the encoding used.
```

I'm not sure what `half price` is, but I assume I'll find a comment in
the codebase that tells me what this is.

I first want to find examples. Failing that, I want to _write_ an example
to understand what this library does.

## `tests/`
I look in the `tests/` folder, with no luck --- I see no examples of how
to use it. Oh well, it's time to read the source.

## Source code structure
running a quick `tree` on the `src/` folder shows:

```
.
└── Data
    ├── Struct
    │   ├── Internal
    │   │   ├── Label.hs
    │   │   ├── LinkCut.hs
    │   │   └── Order.hs
    │   ├── Internal.hs
    │   ├── Label.hs
    │   ├── LinkCut.hs
    │   ├── Order.hs
    │   └── TH.hs
    └── Struct.hs
```

- `Data/Struct.hs` is the toplevel, most likely.
- `Data/Struct/TH.hs` is probably template haskell and can be safely ignored
- `Data/Struct/Label.hs` is probably some kind of labelling / phantom tagging system
- `Data/Struct/Internal/LinkCut.hs` is a link-cut-tree most likely, which is
interesting, since I've never seen anyone use this data structure in the wild.
The only reason I was aware of its existence was thanks to a frind who told me
about "the fucked up algorithms Tarjan came up with". 
- `Data/Struct/Order.hs` is an interesting file, is it a theory of orderings? 

## Exports
```hs
src/Data/Struct.hs
module Data.Struct
  ( Struct(..)
  , Object
  , destruct
  , construct
  , eqStruct
  , alloc
  -- * Nil
#ifndef HLINT
  , pattern Nil
#endif
  , isNil
  , NullPointerException(..)
  -- * Slots and Fields
  , Slot, slot
  , get, set
  , Field, field
  , unboxedField
  , getField, setField, modifyField, modifyField'
  , Precomposable(..)
  ) where

import Data.Struct.Internal
```

Interesting, it seems to provide us a way to construct structs, `get`, `set`
into them, some way to create "struct fields", and helpers such as
`modifyField`.

There's some slightly alarming names there, such as `NullPointerException`
and `isNil`, but I assume we will find out what that is as we go along.


since there's an `import Data.Struct.Internal`, we know that that's where
the implementations are coming from.

Let's see what `get` and `set` do first, since they seem to be
the most useful.

# Spelunking into `Data/Struct/Internal.hs`

```hs
Data/Struct/Internal.hs

-- | Get the value from a 'Slot'
get :: (PrimMonad m, Struct x, Struct y) => Slot x y -> x (PrimState m) -> m (y (PrimState m))
get (Slot go _ _) = \x -> primitive $ \s -> case go (destruct x) s of
                                            (# s', y #) -> (# s', construct y #)

-- | Set the value of a 'Slot'
set :: (PrimMonad m, Struct x, Struct y) => Slot x y -> x (PrimState m) -> y (PrimState m) -> m ()
set (Slot _ go _) = \x y -> primitive_ (go (destruct x) (destruct y))
{-# INLINE set #-}
```

We first see a `Slot` type, and a typeclass `Struct`. So, if we want
to use the `get` or `set` functions, we need to know how to construct
a `Slot`, and how to create a type which has an instance of `Struct`.

Let's look at each of them in turn:

### `Struct`
```hs
-- | A 'Dict' reifies an instance of the constraint @p@ into a value.
data Dict p where
  Dict :: p => Dict p

-- | Run an ST calculation inside of a PrimMonad. This lets us avoid dispatching everything through the 'PrimMonad' dictionary.

-- | An instance for 'Struct' @t@ is a witness to the machine-level
--   equivalence of @t@ and @Object@.
class Struct t where
  struct :: Dict (Coercible (t s) (Object s))
#ifndef HLINT
  default struct :: Coercible (t s) (Object s) => Dict (Coercible (t s) (Object s))
#endif
  struct = Dict

data Object s = Object { runObject :: SmallMutableArray# s Any }

instance Struct Object
```

So, a `Struct` is anything that witnesses the ability to be coerced to
`Object s`.

`Object s` appears to be an array of size `s`? We'll need to investigate
what to do with this. 

However, first things first, I want to see an
example of this thing in action! By looking around a little more,
I find a use of some template haskell that generates a struct in
`Data/Struct/Internal/LinkCut.hs`:

```hs
Data/Struct/Internal/LinkCut.hs:

makeStruct [d|
  data LinkCut a s = LinkCut
    { path, parent, left, right :: !(LinkCut a s)
    , value, summary :: a
    }
   |]
```

## Start of experimentation, or how to learn a library from the inside-out

### `Hello, World` with the library

Now that we have a file that uses the `Struct` library in the form
of `Data/Struct/Internal/LinkCut.hs`, we can begin to experiment. I prefer
to try small examples to get a feeling for the API of a library first.

I add a new test file to the test bench where I can experiment, call
it `test/unit.hs` (for unit tests), and start trying to instantiate
a use of `makeStruct`.

First I try:
```
test/unit.hs

makeStruct [d|
  data TupleInts = TupleInts
    { tupleLeft, tupleRight :: !Int } 
    |]
```

This dies with the error:

```
structs/tests/unit.hs:22:1: error: Unable to match state type of slot
```

After squinting, I notice that `LinkCut` takes two type parameters
`a` and `s`. Guessing that I need my type to be polymorphic over `a` with
a phantom `s`, I change to:

```hs
test/unit.hs

makeStruct [d|
  data TupleInts a s  = TupleInts
    { tupleLeft, tupleRight :: a
    } 
    |]
```

which compiles successfully! Awesome, I now have a `Struct` on my hand... 
I assume. Now, how to I use this thing? Let's go back to `Internal/LinkCut.hs`.

```
Internal/LinkCut.hs

-- | O(1). Allocate a new link-cut tree with a given monoidal summary.
new :: PrimMonad m => a -> m (LinkCut a (PrimState m))
new a = st (newLinkCut Nil Nil Nil Nil a a)
```

I hadn't seem `PrimMonad` before. Hackage reveals it to abstract out any
kind of state monad which can execute primpos on array-like objects.
So, think `PrimMonad ~ ST, IO` etc.

First, let's understand what `st` is doing:
```
-- | Run an ST calculation inside of a PrimMonad. This lets us avoid dispatching everything through the 'PrimMonad' dictionary.
st :: PrimMonad m => ST (PrimState m) a -> m a
st = primToPrim
{-# INLINE[0] st #-}
```
I'm not entirely sure which dictionary this is referring to (presmuably, the
typeclass lookup dictionary of `primMonad m`). I don't see how this
avoids dispatching, so **TODO: understand this later**.

We try to understand `newTupleInts`, so we load our test file in GHCi
and see what the type is:
```

*Main> :t newTupleInts 
newTupleInts
  :: primitive-0.6.3.0:Control.Monad.Primitive.PrimMonad m =>
     a
     -> a
     -> m (TupleInts
             a (primitive-0.6.3.0:Control.Monad.Primitive.PrimState m))
```

Cool, it looks like we can create something! 

Let's try to `set` next. We had spied functions called `set` and `setField`
being used in `LinkCut`, so let's check their types, along with the
type of our field, `tupleLeft`:

```
*Main> :t tupleLeft 
tupleLeft :: Field (TupleInts a) a
*Main> :t setField
setField
  :: (PrimMonad m, Struct x) =>
     Field x a -> x (PrimState m) -> a -> m ()
*Main> :t getField
getField
  :: (PrimMonad m, Struct x) => Field x a -> x (PrimState m) -> m a
*Main> :t set
set
  :: (PrimMonad m, Struct x, Struct y) =>
     Slot x y -> x (PrimState m) -> y (PrimState m) -> m ()
*Main> :t get
get
  :: (PrimMonad m, Struct x, Struct y) =>
     Slot x y -> x (PrimState m) -> m (y (PrimState m))
```

Cool, so it looks like there's a distinction between what is a `Slot`
and what's a `Field`. 

By looking at how `set` and `setField` is used in `LinkCut`, it's easy to
deduce that `{set, get}` is for recursive elements, which should intuitively be
converted to poiners. In the `LinkCut` case, the pointers to `left` and
`right`. 

`{set, get}Field` is meant for raw values stored in the struct.

So, we can write our first test case:

```hs
setTupleLeft :: PrimMonad m => TupleInts a (PrimState m) -> a -> m ()
setTupleLeft tup val = setField tupleLeft tup val

getTupleLeft :: PrimMonad m => TupleInts a (PrimState m) -> m a
getTupleLeft tup = getField tupleLeft tup

...

unitTests = testGroup "Unit tests"
  [ testCase "create and get value from tuple" $ 
      runST $ do
        c <- mkTupleInts 10 20
        val <- getTupleLeft  c
        return (val @?= 10)

  ]
```

A `stack build structs:test:unit` tells us that all the tests pass! Cool,
we now understand basic usage (of at least `get` and `mk`, `set` should be
just as straightforward).

### Creating linked lists

To get some experience with `{get, set}`, let's encode a linked list
and check that it works.

I first tried:
```hs
makeStruct [d|
  data LinkedList a s  = LinkedList
    { val :: a,
      next :: LinkedList a s
    } 
    |]
```

which died with:
```
/home/bollu/work/propogators/hask/structs/tests/unit.hs:36:1: error:
    state type may not occur in field `next`
```

Comparing to `LinkCut`, I realised that I needed my `next` field __to be
strict__. So, the library by-construction forces us to not be able to
create "lazy" data structures. 

A quick exposition of what I did:
```hs

makeStruct [d|
  data LinkedList a s  = LinkedList
    { val :: a,
       next :: !(LinkedList a s) }
    |]

-- Make an empty linked list
mkEmptyLinkedList ::  LinkedList a s
mkEmptyLinkedList = Nil 

-- Make a linked list node with a value
mkLinkedListNode :: PrimMonad m => a -> m (LinkedList a (PrimState m))
mkLinkedListNode a = newLinkedList a Nil

-- Append a node to a linked list.
appendLinkedList :: PrimMonad m => 
  LinkedList x (PrimState m) 
  -> x 
  -> m (LinkedList x (PrimState m))
appendLinkedList xs x = do
  isend <- isNil <$> (get next xs)
  if isend
     then do
       nodex <- mkLinkedListNode x
       set next xs nodex
       return xs
      else do
        xs' <- get next xs
        appendLinkedList xs' x

-- Retreive the nth value from the linked list.
nthLinkedList :: PrimMonad m => Int -> LinkedList a (PrimState m) -> m a
nthLinkedList 0 xs = getField val xs
nthLinkedList i xs = get next xs >>= nthLinkedList (i - 1)

-- Convert a haskell list to a linked list
listToLinkedList :: PrimMonad m => [a] -> m (LinkedList a (PrimState m))
listToLinkedList [] = return mkEmptyLinkedList 
listToLinkedList (x:xs) = do
  head <- mkLinkedListNode x
  rest <- listToLinkedList xs
  set next head rest

  return head


-- TODO: setup ViewPatterns  to check when something is nil
-- concat xs ys ==  xs := xs ++ ys
concatLinkedList :: PrimMonad m => 
      LinkedList a (PrimState m) 
  -> LinkedList a (PrimState m) 
  -> m ()
concatLinkedList xs ys =
  if isNil xs 
     then error "head of list is undefined"
     else do 
       isend <- isNil <$> (get next xs)
       if isend 
           then set next xs ys
           else get next xs >>= \xs' -> concatLinkedList xs' ys


-- Return if a list equal to some linked list representation.
listEqLinkedList :: PrimMonad m => Eq a => [a] -> LinkedList a (PrimState m) -> m Bool
listEqLinkedList [] l = return $ isNil l
listEqLinkedList (x:xs) l = do
  xval <- getField val l
  if xval == x
     then do
       l' <- get next l
       listEqLinkedList xs l'
    else return False


... (test harness setup elided)

-- QuickCheck sanity checks!
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty @ ([Int] -> Bool) "list to linked list" $ 
    \xs -> runST $ do
      lxs <- listToLinkedList xs
      listEqLinkedList xs lxs

  , QC.testProperty @ (NonEmptyList Int -> Bool) "Indexing linked lists" $ 
    \xs -> runST $ do
        lxs <- listToLinkedList (getNonEmpty xs)

        -- TODO: missing Foldable instance for NonEmptyList
        xsAtIx <- sequenceA [nthLinkedList ix lxs | ix <- [0.. length (getNonEmpty xs) - 1]]
        return $ xsAtIx  == getNonEmpty xs

        -- return $ getNonEmpty lxs == xsAtIx

  , QC.testProperty @ (NonEmptyList Int -> [Int] -> Bool) "Appending linked lists" $ 
    \xs ys -> runST $ do
      lxs <- listToLinkedList (getNonEmpty xs)
      lys <- listToLinkedList ys
      
      -- this mutates lxs
      concatLinkedList lxs lys

      listEqLinkedList ((getNonEmpty xs) ++ ys) lxs
  ]
```

So, essentially, we setup a dirt simple linked list library, implement
common functions like indexing and appending, and setup a QuickCheck 
test harness, in ~ 100 lines of code. I quite like being able to use
haskell-isms when writing my imperative code, and this somehow feels
easier than messing with `STRef`s.

## Field accessor nesting

While going through the exports of `Struct`, we notice something called
`Precomposable`:

```hs
*Main> :info Precomposable 
class Precomposable (t :: k -> k1 -> *) where
  (#) :: forall (x :: k) (y :: k) (z :: k1).
         Slot x y -> t y z -> t x z
  {-# MINIMAL (#) #-}
```

So, it looks like it lets us compose `Slot`s to get an inner accessor
slot. Let's try this out:

```
-- Try out the `Precomposable` system
nextnext :: Slot (LinkedList a) (LinkedList a)
nextnext = next # next

nextnextval :: Field (LinkedList a) a
nextnextval = nextnext # val

...

testCase "pull the values out of a linked list using nextnextval" $ runST $ do
  xs <- listToLinkedList [1, 2, 3]
  nnv <- getField nextnextval xs
  return (nnv @?= 3)
```

Yep, the unit test passes, so it simply lets us create new `Field`
and `Slot`s that can drill deeper. Looks like an incantation of `Lens`.

## Wrapping up the spelunking, and going deeper

At this point, we know how to use the library, so let's explore how this
actually works under the hood.

We would ideally like to understand `alloc`, `set/setField/get/getField`,
and why this distinction between `Field` and `Slot` has been drawn.

I also wanted to understand how this interacts with GC, and where the `free`
call is, because AFAICT, I'm just heammoraging memory right now.

### `alloc`


### `Slot`
```hs
-- | A 'Slot' is a reference to another unboxed mutable object.
data Slot x y = Slot
  (forall s. SmallMutableArray# s Any -> State# s -> (# State# s, SmallMutableArray# s Any #))
  (forall s. SmallMutableArray# s Any -> SmallMutableArray# s Any -> State# s -> State# s)
  (forall s. SmallMutableArray# s Any -> SmallMutableArray# s Any -> SmallMutableArray# s Any -> State# s -> (# State# s, Int#, SmallMutableArray# s Any #))

```


```hs
src/Data/Struct/Internal.hs:113
--------------------------------------------------------------------------------
-- * Tony Hoare's billion dollar mistake
--------------------------------------------------------------------------------

-- | Box is designed to mirror object's single field but using the 'Null' type
-- instead of a mutable array. This hack relies on GHC reusing the same 'Null'
-- data constructor for all occurrences. Box's field must not be strict to
-- prevent the compiler from making assumptions about its contents.
data Box = Box Null
data Null = Null
```
