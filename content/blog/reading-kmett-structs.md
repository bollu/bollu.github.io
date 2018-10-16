+++
title = "Reading Kmett's `structs` library"
date = "2018-10-04T19:53:32+05:30"
draft = false

+++

I'm examining Edward Kmett's [structs](https://www.github/com/ekmett/structs)
package.  I decided to write up fun bits of the code I found as I spelunked on
the codebase, as well as motivate why this library exists (as far as I can
tell, anyway).

**Pre-requisites:**  General haskell-familiarity upto monads. In this
case, `IO` and `ST` familiarity is assumed since we're doing mutable things.

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
-- src/Data/Struct.hs
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
-- Data/Struct/Internal.hs

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
-- Data/Struct/Internal.hs

-- | A 'Dict' reifies an instance of the constraint @p@ into a value.
data Dict p where
  Dict :: p => Dict p

-- | Run an ST calculation inside of a PrimMonad. This lets us avoid
-- | dispatching everything through the 'PrimMonad' dictionary.

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
-- Data/Struct/Internal/LinkCut.hs:

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

Looking for it, we find the definition:
```hs
-- Data/Struct/TH.hs
--- 
-- | Generate allocators, slots, fields, unboxed fields, Eq instances,
-- and Struct instances for the given "data types".
--
-- Inputs are expected to be "data types" parameterized by a state
-- type. Strict fields are considered to be slots, Non-strict fields
-- are considered to be boxed types, Unpacked fields are considered
-- to be unboxed primitives.
--
-- The data type should use record syntax and have a single constructor.
-- The field names will be used to generate slot, field, and unboxedField
-- values of the same name.
--
-- An allocator for the struct is generated by prefixing "alloc" to the
-- data type name.
makeStruct :: DecsQ -> DecsQ
```

With that description, I try the definition:
```hs
-- test/unit.hs

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

I hadn't seen `PrimMonad` before. Hackage reveals it to abstract out any
kind of state monad which can execute primpos on array-like objects.
So, think `PrimMonad ~ ST, IO` etc.

We can guess that a function `newTupleInts` has been generated for us
by `makeStruct`, so let's try to use it:

```
-- ghci

*Main> :t newTupleInts 
newTupleInts
  :: primitive-0.6.3.0:Control.Monad.Primitive.PrimMonad m =>
     a
     -> a
     -> m (TupleInts
             a (primitive-0.6.3.0:Control.Monad.Primitive.PrimState m))
```

Cool, it looks like we pass it `left` and `right`, and which
point it create the `m (TupleInts ...)` for us.

Let's try to `set` next. We had spied functions called `set` and `setField`
being used in `LinkCut`, so let's check their types, along with the
type of our field, `tupleLeft`:

```
-- ghci

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
-- tests/unit.hs

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
-- tests/unit.hs

makeStruct [d|
  data LinkedList a s  = LinkedList
    { val :: a,
      next :: LinkedList a s
    } 
    |]
```

which died with:
```
.../unit.hs:36:1: error:
    state type may not occur in field `next`
```

Comparing to `LinkCut`, I realised that I needed my `next` field __to be
strict__. So, the library by-construction forces us to not be able to
create "lazy" data structures. 

A quick exposition of what I did:
```hs
-- tests/unit.hs

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
-- ghci

*Main> :info Precomposable 
class Precomposable (t :: k -> k1 -> *) where
  (#) :: forall (x :: k) (y :: k) (z :: k1).
         Slot x y -> t y z -> t x z
  {-# MINIMAL (#) #-}
```

So, it looks like it lets us compose `Slot`s to get an inner accessor
slot. Let's try this out:

```hs
-- tests/unit.hs

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

### `Struct`

```hs
-- src/Data/Struct/Internal.hs

-- | An instance for 'Struct' @t@ is a witness to the machine-level
--   equivalence of @t@ and @Object@.
class Struct t where
  struct :: Dict (Coercible (t s) (Object s))
#ifndef HLINT
  default struct :: Coercible (t s) (Object s) => Dict (Coercible (t s) (Object s))
#endif
  struct = Dict
  {-# MINIMAL #-}


data Object s = Object { runObject :: SmallMutableArray# s Any }
```

Let's break this down, one-by-one.

An `Object s` is a wrapper around `SmallMutableArray# s Any`, where the
`s` is a state-carrier, and the `Any` is a type that comes from `GHC.Types`,
and can be coerced into _any unlifted type_. So, the type of `Object`
is a heap of bytes which is waiting to be coered into something.

`Dict` is a cool library that lets us reify constraints into data.
For example, `Dict :: Dict (Eq Int)` allows us to witness that
`Int` has an `Eq` instance, and lets us **hold a `Dict` value that witnesses
this**.


`Coercible` comes from `GHC`, and the docs say: "Coercible is a two-parameter
class that has instances for types a and b if the compiler can infer that they
have the same representation"

So now, `Struct` is clear: we use a `Dict` to witness that `Object s` is
`Coercible` to `t s`. Intutively, we're trying to say that our heap of bytes
can be safely reinterpreted at runtime to be `t s`, with nothing going awry in
terms of representation.




### `alloc`

```hs
-- src/Data/Struct/Internal.hs

coerceB :: Dict (Coercible a b) -> b -> a
coerceB Dict = coerce

construct :: Struct t => SmallMutableArray# s Any -> t s
construct = \x -> coerceB struct (Object x)

-- | Allocate a structure made out of `n` slots. Initialize the structure before proceeding!
alloc :: (PrimMonad m, Struct t) => Int -> m (t (PrimState m))
alloc (I# n#) = primitive $ \s -> case newSmallArray# n# undefined s of (# s', b #) -> (# s', construct b #)
```

Here, we make use
```hs
-- Control.Monad.Primitive
primitive :: (State# (PrimState m) -> (#State# (PrimState m), a#)) -> m a
```
which execute a primitive operation of the form `state -> (state, val)` to
create `m val`.

So, `alloc` creates a new arary of `n` `Any` values, and then coerces it to
type `t` with a call to `construct`. 

`construct` uses the `Dict` in `Struct t` to witness the ability to coerce
from `SmallMutableArray# s Any` to `t`.

### `Nil`


```hs
-- src/Data/Struct/Internal.hs:113
--------------------------------------------------------------------------------
-- * Tony Hoare's billion dollar mistake
--------------------------------------------------------------------------------

-- | Box is designed to mirror object's single field but using the 'Null' type
-- instead of a mutable array. This hack relies on GHC reusing the same 'Null'
-- data constructor for all occurrences. Box's field must not be strict to
-- prevent the compiler from making assumptions about its contents.
data Box = Box Null
data Null = Null


-- | Predicate to check if a struct is 'Nil'.
--
-- >>> isNil (Nil :: Object (PrimState IO))
-- True
-- >>> o <- alloc 1 :: IO (Object (PrimState IO))
-- >>> isNil o
-- False
isNil :: Struct t => t s -> Bool
isNil t = isTrue# (unsafeCoerce# reallyUnsafePtrEquality# (destruct t) Null)
{-# INLINE isNil #-}
```
`isNil` appears to take an object, and check if it's the same,
**as a pointer**, to the object `Null`, since GHC reuses data constructors.

```hs
#ifndef HLINT
-- | Truly imperative.
pattern Nil :: Struct t => () => t s
pattern Nil <- (isNil -> True) where
  Nil = unsafeCoerce# Box Null
#endif
```

`Nil` is a pattern synonym, but it's definition escapes me.

I don't understand what the 
```
Nil = unsafeCoerce# Box Null
```


### `Slot`

#### Definition

```hs
src/Data/Struct/Internal.hs:113

-- | A 'Slot' is a reference to another unboxed mutable object.
data Slot x y = Slot
  (forall s. SmallMutableArray# s Any -> State# s -> (# State# s, SmallMutableArray# s Any #))
  (forall s. SmallMutableArray# s Any -> SmallMutableArray# s Any -> State# s -> State# s)
  (forall s. SmallMutableArray# s Any -> SmallMutableArray# s Any -> SmallMutableArray# s Any -> State# s -> (# State# s, Int#, SmallMutableArray# s Any #))

-- | The 'Slot' at the given position in a 'Struct'
slot :: Int {- ^ slot -} -> Slot s t
slot (I# i) = Slot
  (\m s -> readSmallMutableArraySmallArray# m i s)
  (\m a s -> writeSmallMutableArraySmallArray# m i a s)
  (\m o n s -> casSmallMutableArraySmallArray# m i o n s)
```
A slot contains three functions, all with a `(forall s. ...)`,
so we can't assume anything about the `s`.A slot consists of three components:

- a way to read from the struct.
- a way to write into the struct.
- `cas` (compare and swap) for an element in the struct.
- a function `slot`, that on given the index of the slot, creates a reader,
writer, compare-and-swapper into the object.

The API design seems weird at first blush, since I would assume that
the API would look more like `slot`, that is, to be indexed by the position
we are trying to read from.

#### `get`
```hs
src/Data/Struct/Internal.hs

-- | Get the value from a 'Slot'
get :: (PrimMonad m, Struct x, Struct y) => Slot x y -> x (PrimState m) -> m (y (PrimState m))
get (Slot go _ _) = \x -> primitive $ \s -> case go (destruct x) s of
                                            (# s', y #) -> (# s', construct y #)
```

`get` performs:
- `destruct x` to go to `Any`
- calls `go`, which is the getter of the `Slot` to pull out the value
- uses `construct` to conver from `Any` to `y`
- calls `primitive` to package the `(#s, y#)` back into a `m (y (PrimState m))`


#### `set`
```hs
src/Data/Struct/Internal.hs

-- | Set the value of a 'Slot'
set :: (PrimMonad m, Struct x, Struct y) => Slot x y 
        -> x (PrimState m) -> y (PrimState m) -> m ()
set (Slot _ go _) = \x y -> primitive_ (go (destruct x) (destruct y))
{-# INLINE set #-}
```

Set calls `destruct` on both x and y to reduce them to `any`, and then
calls the setter field of the slot.


#### `cas`

```hs
-- src/Data/Struct/Internal.hs

cas :: (PrimMonad m, Struct x, Struct y) => Slot x y 
    -> x (PrimState m) -> y (PrimState m) -> y (PrimState m) 
    -> m (Bool, y (PrimState m))
cas (Slot _ _ go) = \m o n -> primitive $ \s -> case go (destruct m) (destruct o) (destruct n) s of
  (# s', i, r #) -> (# s', (tagToEnum# i :: Bool, construct r) #)
```
### `Field`

#### Definition
```hs
-- src/Data/Struct/Internal.hs

-- | A 'Field' is a reference from a struct to a normal Haskell data type.
data Field x a = Field
  (forall s. SmallMutableArray# s Any -> State# s -> (# State# s, a #)) -- get
  (forall s. SmallMutableArray# s Any -> a -> State# s -> State# s) -- set


-- | Store the reference to the Haskell data type in a normal field
field :: Int {- ^ slot -} -> Field s a
field (I# i) = Field
  (\m s -> unsafeCoerce# readSmallArray# m i s)
  (\m a s -> unsafeCoerce# writeSmallArray# m i a s)
```

Predictably, a `Field` is pretty much the same as a `Slot`. I'm not sure why it
doesn't have compare-and-swap, perhaps because there was no need for it
right now.

#### `get, set`:
Getters and setters are the same as that of `Slot`:

```hs
-- src/Data/Struct/Internal.hs

-- | Get the value of a field in a struct
getField :: (PrimMonad m, Struct x) => Field x a -> x (PrimState m) -> m a
getField (Field go _) = \x -> primitive (go (destruct x))
{-# INLINE getField #-}

-- | Set the value of a field in a struct
setField :: (PrimMonad m, Struct x) => Field x a -> x (PrimState m) -> a -> m ()
setField (Field _ go) = \x y -> primitive_ (go (destruct x) y)
{-# INLINE setField #-}
```

## Understanding `makeStruct`

Now that we know the primitives this library is built on, we can read
how the conversion takes place. In particular, we can first see the
elaborated template haskell from our unit tests (with `-ddump-splice`), and then read the code
that generates the template haskell.


### Elaboration
A run of 
```
stack build structs:test:unit  --ghc-options "-ddump-splices -ddump-to-file"
```

gives me access to the elaborated code.

#### `TupleInts` elboration
```hs
unit.hs

makeStruct [d|
  data TupleInts a s  = TupleInts
    { tupleLeft, tupleRight :: a
    } 
    |]
```


Elaborates to this (only interesting code retained):
```hs
unit.dump-splice

    newtype TupleInts_aa1E a_aa1I s_aa1J
      = TupleInts_aa1F (Object s_aa1J)

    instance Struct (TupleInts_aa1E a_aa1I) where
      struct = Dict

    instance Eq (TupleInts_aa1E a_aa1I s_aa1J) where
      (==) = eqStruct

    tupleLeft_aa1G ::
      forall a_aa1I. Field (TupleInts_aa1E a_aa1I) a_aa1I
    tupleLeft_aa1G = field 0
    {-# INLINE tupleLeft_aa1G #-}

    tupleRight_aa1H ::
      forall a_aa1I. Field (TupleInts_aa1E a_aa1I) a_aa1I
    tupleRight_aa1H = field 1
    ...
    allocTupleInts ::
      forall a_aa1I.
      forall m_aa1O.
      PrimMonad m_aa1O =>
      m_aa1O (TupleInts_aa1E a_aa1I (PrimState m_aa1O))
    allocTupleInts = alloc 2
```
- `TupleInts` elaborates to a `newtype`, wrapping
the previously seen `Object`. 

- This naturally gives it a `Struct` instance.

- the `alloc` call allocates an `Object` that can hold `2` `Any`s,. This 
works since we have two fields which need to be coered into `Int`. (since an
`Int` is a thunked object, it makes sense that it would fit with an `Any`. The
interesting question is what happens when the object is unboxed).

- The rest of it is straightforward, with uses of `field` to
create the nth field constructor.

#### Elaboration of `UNPACKED` fields:

The API said:
```
Unpacked fields are considered to be unboxed primitives.
```
so let's try to elaborate some `UNPACK`ed fields:

```hs
unit.hs

makeStruct [d| data Vec3 s  = Vec3 { x, y, z :: {-# UNPACK #-} !Int  } |]
```

elaborates to:


```hs
unit.dump-splice

    newtype Vec3_acMM s_acMR = Vec3_acMN (Object s_acMR)

    instance Struct Vec3_acMM where
      struct = Dict

    instance Eq (Vec3_acMM s_acMR) where
      (==) = eqStruct

    x_acMO :: Field Vec3_acMM Int
    x_acMO = (unboxedField 0) 0
    {-# INLINE x_acMO #-}

    y_acMP :: Field Vec3_acMM Int
    y_acMP = (unboxedField 0) 1

    {-# INLINE y_acMP #-}
    z_acMQ :: Field Vec3_acMM Int
    z_acMQ = (unboxedField 0) 2

    allocVec3 ::
      forall m_acMY.
      PrimMonad m_acMY => m_acMY (Vec3_acMM (PrimState m_acMY))
    allocVec3 = alloc 1
```

So, `unboxedField` is used to read the unboxed fields. Looking
at the implementation, this is:

```hs
-- src/Data/Struct/Internal.hs

-- | Store the reference in the nth slot in the nth argument, treated as a MutableByteArray
unboxedField :: Prim a => Int {- ^ slot -} -> Int {- ^ argument -} -> Field s a
unboxedField (I# i) (I# j) = Field
  (\m s -> case readMutableByteArraySmallArray# m i s of
     (# s', mba #) -> readByteArray# mba j s')
  (\m a s -> case readMutableByteArraySmallArray# m i s of
     (# s', mba #) -> writeByteArray# mba j a s')
```

So, the elaboration elaborates all our unboxed fields into one
boxed reference to `mutableByteSmallArray`.

Looking through `Internal.hs`, we discover:
```hs
-- src/Data/Struct/Internal.hs

-- | Initialized the mutable array used by 'unboxedField'. Returns the array
-- after storing it in the struct to help with initialization.
initializeUnboxedField ::
  (PrimMonad m, Struct x) =>
  Int             {- ^ slot     -} ->
  Int             {- ^ elements -} ->
  Int             {- ^ element size -} ->
  x (PrimState m) {- ^ struct   -} ->
  m (MutableByteArray (PrimState m))
initializeUnboxedField (I# i) (I# n) (I# z) m =
  primitive $ \s ->
    case newByteArray# (n *# z) s of
      (# s1, mba #) ->
        (# writeMutableByteArraySmallArray# (destruct m) i mba s1, MutableByteArray mba #)
```

So, the guess is that it collects elements of the same type into one
array, which it then subdivides among members.



### Code

```hs
Data/Struct/TH.hs

-- | Generate allocators, slots, fields, unboxed fields, Eq instances,
-- and Struct instances for the given "data types".
--
-- Inputs are expected to be "data types" parameterized by a state
-- type. Strict fields are considered to be slots, Non-strict fields
-- are considered to be boxed types, Unpacked fields are considered
-- to be unboxed primitives.
--
-- The data type should use record syntax and have a single constructor.
-- The field names will be used to generate slot, field, and unboxedField
-- values of the same name.
--
-- An allocator for the struct is generated by prefixing "alloc" to the
-- data type name.
makeStruct :: DecsQ -> DecsQ
makeStruct dsq =
  do ds   <- dsq
     (passthrough, reps) <- partitionEithers <$> traverse computeRep ds
     ds's <- traverse (generateCode passthrough) reps
     return (passthrough ++ concat ds's)
```

**question:** The API is puzzling, why is there is seemingly arbitrary mapping 
between the different kinds of fields we have in haskell, to types of fields
we would have in the `struct`-ified version? If it is arbitrary, then
maybe writing a source plugin with custom annotations would be cleaner.

anyway, the function `makeStuct` is splitting stuff up based on `computeRep`,
generating code for everything that passed, and then returning the modified
`ds's`, with the rest of the `passthrough`.

```hs
-- Data/Struct/TH.hs

computeRep :: Dec -> Q (Either Dec StructRep)
computeRep (DataD c n vs _ cs ds) =
  do state <- validateStateType vs
     (conname, confields) <- validateContructor cs
     members <- traverse (validateMember state) confields

     return $ Right StructRep
       { srState = state
       , srName  = n
       , srTyVars = vs
       , srConstructor = conname
       , srMembers = members
       , srDerived = ds
       , srCxt = c
       }
computeRep d = return (Left d)
```

So, it's validating a bunch of stuff in the constructor, and then
returning a `StructRep`, which represents the structure to be generated,
or the original `Dec`.


```hs
-- Data/Struct/TH.hs

data StructRep = StructRep
  { srState       :: Name
  , srName        :: Name
  , srTyVars      :: [TyVarBndr]
  , srDerived     :: [DerivClause]
  , srCxt         :: Cxt
  , srConstructor :: Name
  , srMembers     :: [Member]
  } deriving Show

data Member = Member
  { _memberRep :: Representation
  , memberName :: Name
  , _memberType :: Type
  }
  deriving Show
```

So, it looks straightforward, just containing all the fields we would expect.
Let's look at `generateCode`, to see if anything more interesting happens.

```hs
-- Data/Struct/TH.hs

generateCode :: [Dec] -> StructRep -> DecsQ
generateCode ds rep = concat <$> sequence
  [ generateDataType rep
  , generateStructInstance rep
  , generateMembers rep
  , generateNew rep
  , generateAlloc rep
  , generateRoles ds rep
  ]
```

It sequences all the `generate*` calls. This is sensible, and we now
understand what the generated code looks like.

# Conclusion

This was a whirlwind tour of the `structs` library. I found it really
interesting to learn a library from the inside-out, and I hope this managed to
convey some of the flavour.

The techniques used in this library are interesting, and I want to benchmark
this and see if this can buy some something in terms of "real" performance.

[I've opened a PR against
`ekmett/structs`](https://github.com/ekmett/structs/pull/13) adding some tests
and examples.  I think reviewing packages that are
interesting, but have sparse information on them is a cool way to give back!
