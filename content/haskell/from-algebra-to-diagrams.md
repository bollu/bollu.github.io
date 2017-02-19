+++
Categories = ["haskell"]
Description = ""
Tags = ["haskell"]
date = "2016-01-14T19:28:52+05:30"
title = "from algebra to diagrams"

draft=true

+++

## Building a Cellular Automata

## History of Cellular Automata

## A mental model of a Cellular Automata

Let's first mockup what a Cellular Automata would look like as a Haskell datatype.

Considering that we want to start with something simple, I'm constructing an automata with just three rules in 
1D space

1. Every cell is either alive (white) or dead (black)
2. If a cell has a live neighbor, it lives
2. If a cell has two live neighbors, it dies
3. Otherwise, it continues in its previous state

An example of the CA behavior is here:

# Fill this in!

To model this, we could use:

```haskell
data Cell = On | Off
data CA = CA {
    cells :: [Cell]
}
```

Next, we would need some way to propagate the state of the cellular automata.
The natural way would be to try and design a function that we can use to
`fmap` over the `cells` of the CA.

```haskell
-- not possible, we need *context* (neighbors) as well!
stepCell :: Cell -> Cell
stepCell = undefined 

stepCA :: CA -> CA
stepCA ca = fmap stepCell (cells ca)
```

So clearly, we need a way to propagate state to the `stepCell` function along with context.

Haskell has a slightly non-obvious-at-first-glance solution to these kinds of situations where
we need to "focus" on a structure called as as a `Zipper`.

A `Zipper` for a `List` is of the form.

```haskell
data Zipper a = Zipper {
    left :: [a],
    focus :: a,
    right :: [a]
}
```
For example, the list

```haskell
[1, 2, 3, (4), 5, 6, 7]
```
where `4` is the element in focus will have the zipper 

```haskell
z = Zipper Int {
    left :: [3, 2, 1], -- stored in order of closeness to focus
    focus :: 4,
    right :: [5, 6, 7]
}
```
Notice that we store the left in such a way that the element *closest* to the focus
has index 0. 

So, if we choose to re-write our `CA` in the form of a zipper, it becomes
```haskell
data Zipper a = Zipper {
    left :: [a],
    focus :: a,
    right :: [a]
}

data Cell = On | Off deriving(Eq)
type CA = Zipper Cell

-- notice the changed definition
-- it now takes the *entire* CA, and updates returns a cell
stepCell :: CA -> Cell
stepCell ca = next,
    leftCell = if (null left) then Off else (left !! 0)
    rightCell = if (null right) then Off else (right !! 0)
    counter n = if n == On then 1 else 0
    liveNeighbours = counter leftCell + counter rightCell
    next = if liveNeighbours == 1 then On
             else if liveNeighbours == 2 then Off
             else focus

-- how do we define this now?
stepCA :: CA -> CA
stepCA ca = undefined
```
Now, we clearly have a problem - our `stepCell :: CA -> Cell` can now
update the CA at *one* position, but we need to update it at *all* positions.

With some type level magic, we can start to think of the problem this way:
```haskell
CA :: Zipper Cell

-- have a rule to update the target
stepCA :: Zipper Cell -> Cell

-- have a rule to generate "all universe of universes"
duplicateCA :: Zipper Cell -> Zipper (Zipper Cell)

-- map the step rule over all possible combinations of universes
fmap stepCA (duplicateCA ca) :: Zipper Cell
```
The way to think about ```Zipper (Zipper Cell)``` is a type that is focused on *one* `Zipper Cell` (that is,
one particular `CA`), while having "translates" of the `CA` in its `before` and `after`.


# Enter the Comonad

The typelcass that describes the structure that we are looking for is called as a `Comonad`. Unfortunately,
it requires some time to "get to the point" of the typeclass, but once we do, it's totally worth it.
So let's jump right in!

```haskell
type Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
```
Let's break this down and see what the two functions of the `Comonad` are doing

```haskell
extract :: w a -> a
```
clearly takes a context `w a` and focuses on a part of it (which is what our `Zipper` does).


```haskell
duplicate :: w a -> w (w a)
```
Seems to take a context `w a` and then build up a "context of contexts"? How in the world do we 
interpret this?

