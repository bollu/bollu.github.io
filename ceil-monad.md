If anyone is interested in the "fleshed out version" of the 'ceiling gives us a monad':

We are thinking of building analog machines, which run on analog data [pure]. Unfortunately, the world is digital, so we can only interact with them by discretizing input and output [impure I/O]. This gives us the following types:


```hs
-- | naming convention
-- |allows us to use f for function
-- | and r for real w/o confusion
type Real = Float;

-- | inject the integers into the reals
-- | inject(floor(r)) <= r for real
-- | inject(floor(i)) <= i for int
inject :: Int -> Real
inject = GHC.Real.fromIntegral
-- | floor(inject(i)) = i
floor :: Real -> Int
floor = GHC.Real.floor
```

That's the data for out adjunction above. Now, the collection of analog programs  is represented by:

```hs
-- | Programs on an analog machine
type Prog = (Float -> Float)
```

To run an analog machine, we give it an `Int` input, and expect an `Int` output.
```hs
-- | Run an analog machine pipeline
-- | with discrete inputs and outputs
runProgram :: Prog -> (Int -> Int)
runProgram p = floor . p . inject
```

We need to be able to sequence these analog machines together [the hallmark
of a monad, after all]. But we can't sequence them in any old way: we need
to connect them with **digital wires** which will discretize their outputs.
Such is the structure of our world, since being able to run code
perfectly on analog machines allows us to solve the halting problem!


```hs
-- | sequence two analog machines
-- with a digital wire. 
-- Data will
-- be discretized between
-- [machine f] and [machine g]
(>=>) :: Prog -> Prog -> Prog
(>=>) f g = (f . inject . floor . g)
```

Note that the discretization is given by `inject . floor`: That is,
floor down a real valued input to an `int`, and then make it a `Real` again
with an `inject`.

Finally, we implement the odds and ends a `Monad` asks us for:

```hs
return :: Float -> Prog; return r = \ignore -> r
```
a `return` analog machine ignores whatever input you feed it, and just 
outputs a constant value.

```hs
-- | identity analog machine, does nothing.
idProg :: Prog; idProg = (\r -> r)
```
The identity analog machine outputs whatever input was fed into it.

We can check that the monad laws are satisfied (in terms of `>=>`). See that
associativity is immediately satisfied, because we have defined `>=>` using
function composition (`(.)`) which is associative.

```
1. Left identity:
runProgram (idProg >=> p) i = runProgram p i

runProgram (idProg >=> p) i
[substitute >=>, runProgram]
  := floor . (idProg . inject . floor . p) . inject i
[idProg = identity function]
  := floor . (inject . floor . p) . inject i
[idProg = identity function]
  := floor . (inject . floor . p) . inject i
[associativity]
  := (floor . inject) . floor . p . inject i
[floor(inject(i)) = i]
  :=  floor . p . inject i
[defn of runProgram]
  :=  runProgram p i
```

Similarly, we can prove right identity:

```
2. Right identity
runProgram (p >=> idProg) i = runProgram p i

runProgram (p >=> idProg) i
[substitute >=>, runProgram]
  := floor . (p . inject . floor . idProg) . inject i
[idProg = identity function, can be removed]
  := floor . (p . inject . floor) . inject i
[associativity]
  := (floor . p . inject) . (floor . inject) i
[floor(inject(i)) = i]
  := (floor . p . inject) i
[defn of runProgram]
  :=  runProgram p i
````
So, we have a monoid `>=>` with identity `idProg`. Hence, we have a monad.
Our monad is the monad of analog computations, with impurity coming from
the discretization given by the external world. We can check that this
is indeed the case:

```hs
*Main> [runProgram (sin >=> cos) x | x <- [0..10]]
[1,1,1,1,0,0,0,1,1,1,0]
*Main> [(floor.cos.inject.floor.sin.inject$x) | x <- [0..10]]
[1,1,1,1,0,0,0,1,1,1,0]
```
