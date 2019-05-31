Contents of [pixel-druid.com](http://pixel-druid.com/), mirrored at [bollu.github.io](http://bollu.github.io/)


The idea is for the website to contain blog posts, along with visualizations of
math / graphics / programming.

The former has been semi-forced thanks to GSoC, as for the latter, it remains
to be seen. I'm hopeful, though :)

# Ideas I stumble onto

# Hamiltonian monte carlo, leapfrog integrators, and sympletic geometry

This is a section that I'll update as I learn more about the space, since I'm studying
differential geometry over the summer, I hope to know enough about "sympletic manifolds".
I'll make this an append-only log to add to the section as I understand more.

##### 31st May

- To perform hamiltonian monte carlo, we use the hamiltonian and its derivatives to provide
a momentum to our proposal distribution --- That is, when we choose a new point from the
current point, our probability distribution for the new point is influenced by our
current momentum

- For some integral necessary within this scheme, Euler integration doesn't cut it 
since the error diverges to infinity

- Hence, we need an integrator that guarantees that the energy of out system is conserved.
Enter the leapfrog integrator. This integrator is also _time reversible_ -- We can run it
forward for `n` steps, and then run it backward for `n` steps to arrive at the same state.
Now I finally know how Braid was implemented, something that bugged the hell out of 9th grade me
when I tried to implement Braid-like physics in my engine!

- The actual derivation of the integrator uses Lie algebras, Sympletic geometry, and other
diffgeo ideas, which is great, because it gives me motivation to study differential geometry `:)`

- Original paper: [Construction of higher order sympletic integrators](https://www.sciencedirect.com/science/article/abs/pii/0375960190900923)

- 

# Small Haskell MCMC implementation:

We create a simple monad called `PL` which allows for a single operation: sampling
from a uniform distribution. We then exploit this to implement MCMC using metropolis hastings,
which is used to sample from arbitrary distributions. Bonus is a small library to render sparklines
in the CLI.

For next time:

- Using applicative to speed up computations by exploiting parallelism
- Conditioning of a distribution wrt a variable

### Source code
```hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
import System.Random
import Data.List(sort, nub)
import Data.Proxy
import Control.Monad (replicateM)
import qualified Data.Map as M


-- | Loop a monadic computation.
mLoop :: Monad m =>
      (a -> m a) -- ^ loop
      -> Int -- ^ number of times to run
      -> a -- initial value
      -> m a -- final value
mLoop _ 0 a = return a
mLoop f n a = f a >>= mLoop f (n - 1)


-- | Utility library for drawing sparklines

-- | List of characters that represent sparklines
sparkchars :: String
sparkchars = "_▁▂▃▄▅▆▇█"

-- Convert an int to a sparkline character
num2spark :: RealFrac a => a -- ^ Max value
  -> a -- ^ Current value
  -> Char
num2spark maxv curv =
   sparkchars !!
     (floor $ (curv / maxv) * (fromIntegral (length sparkchars - 1)))

series2spark :: RealFrac a => [a] -> String
series2spark vs =
  let maxv = if null vs then 0 else maximum vs
  in map (num2spark maxv) vs

seriesPrintSpark :: RealFrac a => [a] -> IO ()
seriesPrintSpark = putStrLn . series2spark

-- Probabilites
-- ============
type F = Float
-- | probablity density
newtype P = P { unP :: Float } deriving(Num)

-- | prob. distributions over space a
newtype D a = D { runD :: a -> P }

uniform :: Int -> D a
uniform n =
  D $ \_ -> P $ 1.0 / (fromIntegral $ n)

(>$<) :: Contravariant f => (b -> a) -> f a  -> f b
(>$<) = cofmap

instance Contravariant D where
  cofmap f (D d) = D (d . f)

-- | Normal distribution with given mean
normalD :: Float ->  D Float
normalD mu = D $ \f -> P $ exp (- ((f-mu)^2))

-- | Distribution that takes on value x^p for 1 <= x <= 2.  Is normalized
polyD :: Float -> D Float
polyD p = D $ \f -> P $ if 1 <= f && f <= 2 then (f ** p) * (p + 1) / (2 ** (p+1) - 1) else 0

class Contravariant f where
  cofmap :: (b -> a) -> f a -> f b

data PL next where
    Ret :: next -> PL next -- ^ return  a value
    Sample01 :: (Float -> PL next) -> PL next -- ^ sample uniformly from a [0, 1) distribution

instance Monad PL where
  return = Ret
  (Ret a) >>= f = f a
  (Sample01 float2plnext) >>= next2next' =
      Sample01 $ \f -> float2plnext f >>= next2next'

instance Applicative PL where
    pure = return
    ff <*> fx = do
        f <- ff
        x <- fx
        return $ f x

instance Functor PL where
    fmap f plx = do
         x <- plx
         return $ f x

-- | operation to sample from [0, 1)
sample01 :: PL Float
sample01 = Sample01 Ret


-- | Run one step of MH on a distribution to obtain a (correlated) sample
mhStep :: (a -> Float) -- ^ function to score sample with, proportional to distribution
  -> (a -> PL a) -- ^ Proposal program
  -> a -- current sample
  -> PL a
mhStep f q a = do
 	a' <- q a
 	let alpha = f a' / f a -- acceptance ratio
 	u <- sample01
 	return $ if u <= alpha then a' else a

-- Typeclass that can provide me with data to run MCMC on it
class MCMC a where
    arbitrary :: a
    uniform2val :: Float -> a

instance MCMC Float where
	arbitrary = 0
	-- map [0, 1) -> (-infty, infty)
	uniform2val v = tan (-pi/2 + pi * v)


{-
-- | Any enumerable object has a way to get me the starting point for MCMC
instance (Bounded a, Enum a) => MCMC a where
     arbitrary = toEnum 0
     uniform2val v = let
        maxf = fromIntegral . fromEnum $ maxBound
        minf = fromIntegral . fromEnum $ minBound
        in toEnum $ floor $ minf + v * (maxf - minf)
-}


-- | Run MH to sample from a distribution
mh :: (a -> Float) -- ^ function to score sample with
 -> (a -> PL a) -- ^ proposal program
 -> a -- ^ current sample
 -> PL a
mh f q a = mLoop (mhStep f q) 100  $ a

-- | Construct a program to sample from an arbitrary distribution using MCMC
mhD :: MCMC a => D a -> PL a
mhD (D d) =
    let
      scorer = (unP . d)
      proposal _ = do
        f <- sample01
        return $ uniform2val f
    in mh scorer proposal arbitrary


-- | Run the probabilistic value to get a sample
sample :: RandomGen g => g -> PL a -> (a, g)
sample g (Ret a) = (a, g)
sample g (Sample01 f2plnext) = let (f, g') = random g in sample g' (f2plnext f)


-- | Sample n values from the distribution
samples :: RandomGen g => Int -> g -> PL a -> ([a], g)
samples 0 g _ = ([], g)
samples n g pl = let (a, g') = sample g pl
                     (as, g'') = samples (n - 1) g' pl
                 in (a:as, g'')

-- | count fraction of times value occurs in list
occurFrac :: (Eq a) => [a] -> a -> Float
occurFrac as a =
    let noccur = length (filter (==a) as)
        n = length as
    in (fromIntegral noccur) / (fromIntegral n)

-- | Produce a distribution from a PL by using the sampler to sample N times
distribution :: (Eq a, Num a, RandomGen g) => Int -> g -> PL a -> (D a, g)
distribution n g pl =
    let (as, g') = samples n g pl in (D (\a -> P (occurFrac as a)), g')


-- | biased coin
coin :: Float -> PL Int -- 1 with prob. p1, 0 with prob. (1 - p1)
coin p1 = do
    Sample01 (\f -> Ret $ if f < p1 then 1 else 0)


-- | Create a histogram from values.
histogram :: Int -- ^ number of buckets
          -> [Float] -- values
          -> [Int]
histogram nbuckets as =
    let
        minv :: Float
        minv = minimum as
        maxv :: Float
        maxv = maximum as
        -- value per bucket
        perbucket :: Float
        perbucket = (maxv - minv) / (fromIntegral nbuckets)
        bucket :: Float -> Int
        bucket v = floor (v / perbucket)
        bucketed :: M.Map Int Int
        bucketed = foldl (\m v -> M.insertWith (+) (bucket v) 1 m) mempty as
     in map snd . M.toList $ bucketed


printSamples :: (Real a, Eq a, Ord a, Show a) => String -> [a] -> IO ()
printSamples s as =  do
    putStrLn $ "***" <> s
    putStrLn $ "   samples: " <> series2spark (map toRational as)

printHistogram :: [Float] -> IO ()
printHistogram samples = putStrLn $ series2spark (map fromIntegral . histogram 10 $  samples)


-- | Given a coin bias, take samples and print bias
printCoin :: Float -> IO ()
printCoin bias = do
    let g = mkStdGen 1
    let (tosses, _) = samples 100 g (coin bias)
    printSamples ("bias: " <> show bias) tosses



-- | Create normal distribution as sum of uniform distributions.
normal :: PL Float
normal =  fromIntegral . sum <$> (replicateM 5 (coin 0.5))


main :: IO ()
main = do
    printCoin 0.01
    printCoin 0.99
    printCoin 0.5
    printCoin 0.7

    putStrLn $ "normal distribution using central limit theorem: "
    let g = mkStdGen 1
    let (nsamples, _) = samples 1000 g normal
    -- printSamples "normal: " nsamples
    printHistogram nsamples


    putStrLn $ "normal distribution using MCMC: "
    let (mcmcsamples, _) = samples 1000 g (mhD $  normalD 0.5)
    printHistogram mcmcsamples

    putStrLn $ "sampling from x^4 with finite support"
    let (mcmcsamples, _) = samples 1000 g (mhD $  polyD 4)
    printHistogram mcmcsamples
```

### Output

```
***bias: 1.0e-2
   samples: ________________________________________█_█_________________________________________________________
***bias: 0.99
   samples: ████████████████████████████████████████████████████████████████████████████████████████████████████
***bias: 0.5
   samples: __█____█__███_███_█__█_█___█_█_██___████████__█_████_████_████____██_█_██_____█__██__██_██____█__█__
***bias: 0.7
   samples: __█__█_█__███_█████__███_█_█_█_██_█_████████__███████████_████_█_███_████_██__█_███__██_███_█_█__█_█
normal distribution using central limit theorem: 
_▄▇█▄_
normal distribution using MCMC: 
__▁▄█▅▂▁___
sampling from x^4 with finite support
▁▁▃▃▃▄▅▆▇█_
```

# The smallest implementation of reverse mode AD (autograd) ever:

```hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Data.Map.Strict as M

-- | This file can be copy-pasted and will run!

-- | Symbols
type Sym = String
-- | Environments
type E a = M.Map Sym a
-- | Newtype to represent deriative values
type F = Float
newtype Der = Der { under :: F } deriving(Show, Num)

infixl 7 !#
-- | We are indexing the map at a "hash" (Sym)
(!#) :: E a -> Sym -> a
(!#) = (M.!)

-- | A node in the computation graph
data Node = 
  Node { name :: Sym -- ^ Name of the node
       , ins :: [Node] -- ^ inputs to the node
       , out :: E F -> F -- ^ output of the node
       , der :: (E F, E (Sym -> Der)) 
                  -> Sym -> Der -- ^ derivative wrt to a name
       }

-- | @ looks like a "circle", which is a node. So we are indexing the map
-- at a node.
(!@) :: E a -> Node -> a 
(!@) e node = e M.! (name node)

-- | Given the current environments of values and derivatives, compute
-- | The new value and derivative for a node.
run_ :: (E F, E (Sym -> Der)) -> Node -> (E F, E (Sym -> Der))
run_ ein (Node name ins out der) = 
  let (e', ed') = foldl run_ ein ins -- run all the inputs
      v = out e' -- compute the output
      dv = der (e', ed') -- and the derivative
  in (M.insert name v e', M.insert name dv ed')  -- and insert them

-- | Run the program given a node 
run :: E F -> Node -> (E F, E (Sym -> Der))
run e n = run_ (e, mempty) n

-- | Let's build nodes
nconst :: Sym -> F -> Node
nconst n f = Node n [] (\_ -> f) (\_ _ -> 0)

-- | Variable
nvar :: Sym -> Node 
nvar n = Node n [] (!# n) (\_ n' -> if n == n' then 1 else 0)
  
-- | binary operation
nbinop :: (F -> F -> F)  -- ^ output computation from inputs
 -> (F -> Der -> F -> Der -> Der) -- ^ derivative computation from outputs
 -> Sym -- ^ Name
 -> (Node, Node) -- ^ input nodes
 -> Node
nbinop f df n (in1, in2) = 
  Node { name = n
       , ins = [in1, in2]
       , out = \e -> f (e !# name in1) (e !# name in2)
       , der = \(e, ed) n' -> 
                 let (name1, name2) = (name in1, name in2)
                     (v1, v2) = (e !# name1, e !# name2)
                     (dv1, dv2) = (ed !# name1 $ n', ed !# name2 $ n')
                     in df v1 dv1 v2 dv2
       }

nadd :: Sym -> (Node, Node) -> Node
nadd = nbinop (+) (\v dv v' dv' -> dv + dv')

nmul :: Sym -> (Node, Node) -> Node
nmul = nbinop (*) (\v (Der dv) v' (Der dv') -> Der $ (v*dv') + (v'*dv))

main :: IO ()
main = do
  let x = nvar "x" :: Node
  let y = nvar "y"
  let xsq = nmul "xsq" (x, x)
  let ten = nconst "10" 10
  let xsq_plus_10 = nadd "xsq_plus_10" (xsq, ten)
  let xsq_plus_10_plus_y = nadd "xsq_plus_10_plus_y"  (xsq_plus_10, y)
  let (e, de) = run (M.fromList $ [("x", 2.0), ("y", 3.0)]) xsq_plus_10_plus_y
  putStrLn $ show e
  putStrLn $ show $ de !@ xsq_plus_10_plus_y $ "x"
  putStrLn $ show $ de !@ xsq_plus_10_plus_y $ "y"
```

Yeah, in ~80 lines of code, you can basically build an autograd engine. Isn't
haskell so rad?


# Timings of passes in GHC, and low hanging fruit in the backend:

- One can use `-v3` to get pass timings.
- Apparently, GHC spends a lot of time in the simplifier, and time
  spend in the backend is peanuts in comparison to this.
- To quote `AndreasK`:
> - Register allocation, common block elimination, block layout and pretty printing are the "slow" things in the backend as far as I remember.
> - There are also a handful of TODO's in the x86 codegen which still apply. So you can try to grep for these.
> - Strength reduction for division by a constant
- [NCG generates slow loop code](https://gitlab.haskell.org/ghc/ghc/issues/9041)
# Varargs in GHC: `ghc/testsuite/tests/rts/T7160.hs`

A comment from this test case tells us why the function `debugBelch2` exists:

```hs
ghc/testsuite/tests/rts/T7160.hs
-- Don't use debugBelch() directly, because we cannot call varargs functions
-- using the FFI (doing so produces a segfault on 64-bit Linux, for example).
-- See Debug.Trace.traceIO, which also uses debugBelch2.
foreign import ccall "&debugBelch2" fun :: FunPtr (Ptr () -> Ptr () -> IO ())
```

The implementation is:

```c
ghc/libraries/base/cbits/PrelIOUtils.c

void debugBelch2(const char*s, char *t)
{
    debugBelch(s,t);
}
```

```
ghc/rts/RtsMessages.c

RtsMsgFunction *debugMsgFn  = rtsDebugMsgFn;
...

void
debugBelch(const char*s, ...)
{
  va_list ap;
  va_start(ap,s);
  (*debugMsgFn)(s,ap);
  va_end(ap);
}
```
# Debugging debug info in GHC: [Link](https://github.com/ghc/ghc/blob/535a26c90f458801aeb1e941a3f541200d171e8f/compiler/cmm/Debug.hs#L458)

I wanted to use debug info to help build a better deubgging experience
within [`tweag/asterius`](http://github.com/tweag/asterius). So, I was 
reading through the sources of `cmm/Debug.hs`.  
I'd never considered how to debug debug-info, and I found the information
tucked inside a cute note in GHC (`Note [Debugging DWARF unwinding info]`):

> This makes GDB produce a trace of its internal workings. Having gone this far,
> it's just a tiny step to run GDB in GDB. Make sure you install debugging
> symbols for gdb if you obtain it through a package manager.


# How does GHC `show` a `Float`?

Turns out the implementation uses a cool paper:

> Printing float point numbers fast and accurately

##### `class Show`:

First, let's review the `Show` typeclass:

```hs
class Show a where
    showsPrec :: Int    -- ^ the operator precedence of the enclosing
                        -- context (a number from @0@ to @11@).
                        -- Function application has precedence @10@.
              -> a      -- ^ the value to be converted to a 'String'
              -> ShowS

    -- | A specialised variant of 'showsPrec', using precedence context
    -- zero, and returning an ordinary 'String'.
    show      :: a   -> String
    ... 
    show x          = shows x ""

-- | equivalent to 'showsPrec' with a precedence of 0.
shows           :: (Show a) => a -> ShowS
shows           =  showsPrec 0
```
So, note that all we need to do is to define `showsPrec`, and `show`
gets defined as `show x = showsPrec 0 x ""`

##### `instance Show Float`:

```hs
instance  Show Float  where
    showsPrec   x = showSignedFloat showFloat x
```

##### `showSignedFloat`:

`showSignedFloat` is a combinator to care of printing signs:

```hs
showSignedFloat :: (RealFloat a)
  => (a -> ShowS)       -- ^ a function that can show unsigned values
  -> Int                -- ^ the precedence of the enclosing context
  -> a                  -- ^ the value to show
  -> ShowS
showSignedFloat showPos p x
   | x < 0 || isNegativeZero x
       = showParen (p > 6) (showChar '-' . showPos (-x))
   | otherwise = showPos x
```

##### `showFloat`:

the `showFloat` is the function that is able to show a float as an
unsigned value.


```hs
showFloat :: (RealFloat a) => a -> ShowS
showFloat x  =  showString (formatRealFloat FFGeneric Nothing x)
```

```hs
-- | utility function converting a 'String' to a show function that
-- simply prepends the string unchanged.
showString      :: String -> ShowS
showString      =  (++)
```

##### `formatRealFloat`:

```hs
formatRealFloat :: (RealFloat a) => FFFormat -> Maybe Int -> a -> String
formatRealFloat fmt decs x = formatRealFloatAlt fmt decs False x

formatRealFloatAlt :: (RealFloat a) => FFFormat -> Maybe Int -> Bool -> a
                 -> String
formatRealFloatAlt fmt decs alt x
   | isNaN x                   = "NaN"
   | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
   | x < 0 || isNegativeZero x = '-':doFmt fmt (floatToDigits (toInteger base) (-x))
   | otherwise                 = doFmt fmt (floatToDigits (toInteger base) x)
 where
  base = 10

  doFmt format (is, e) =
    let ds = map intToDigit is in
    case format of
     FFGeneric ->
      doFmt (if e < 0 || e > 7 then FFExponent else FFFixed)
            (is,e)
     FFExponent ->
      case decs of
       Nothing ->
        let show_e' = show (e-1) in
        case ds of
          "0"     -> "0.0e0"
          [d]     -> d : ".0e" ++ show_e'
          (d:ds') -> d : '.' : ds' ++ "e" ++ show_e'
          []      -> errorWithoutStackTrace "formatRealFloat/doFmt/FFExponent: []"
       Just d | d <= 0 ->
        -- handle this case specifically since we need to omit the
        -- decimal point as well (#15115).
        -- Note that this handles negative precisions as well for consistency
        -- (see #15509).
        case is of
          [0] -> "0e0"
          _ ->
           let
             (ei,is') = roundTo base 1 is
             n:_ = map intToDigit (if ei > 0 then init is' else is')
           in n : 'e' : show (e-1+ei)
       Just dec ->
        let dec' = max dec 1 in
        case is of
         [0] -> '0' :'.' : take dec' (repeat '0') ++ "e0"
         _ ->
          let
           (ei,is') = roundTo base (dec'+1) is
           (d:ds') = map intToDigit (if ei > 0 then init is' else is')
          in
          d:'.':ds' ++ 'e':show (e-1+ei)
     FFFixed ->
      let
       mk0 ls = case ls of { "" -> "0" ; _ -> ls}
      in
      case decs of
       Nothing
          | e <= 0    -> "0." ++ replicate (-e) '0' ++ ds
          | otherwise ->
             let
                f 0 s    rs  = mk0 (reverse s) ++ '.':mk0 rs
                f n s    ""  = f (n-1) ('0':s) ""
                f n s (r:rs) = f (n-1) (r:s) rs
             in
                f e "" ds
       Just dec ->
        let dec' = max dec 0 in
        if e >= 0 then
         let
          (ei,is') = roundTo base (dec' + e) is
          (ls,rs)  = splitAt (e+ei) (map intToDigit is')
         in
         mk0 ls ++ (if null rs && not alt then "" else '.':rs)
        else
         let
          (ei,is') = roundTo base dec' (replicate (-e) 0 ++ is)
          d:ds' = map intToDigit (if ei > 0 then is' else 0:is')
         in
         d : (if null ds' && not alt then "" else '.':ds')
```

##### `floatToDigits`:

```hs
-- Based on "Printing Floating-Point Numbers Quickly and Accurately"
-- by R.G. Burger and R.K. Dybvig in PLDI 96.
-- This version uses a much slower logarithm estimator. It should be improved.

-- | 'floatToDigits' takes a base and a non-negative 'RealFloat' number,
-- and returns a list of digits and an exponent.
-- In particular, if @x>=0@, and
--
-- > floatToDigits base x = ([d1,d2,...,dn], e)
--
-- then
--
--      (1) @n >= 1@
--
--      (2) @x = 0.d1d2...dn * (base**e)@
--
--      (3) @0 <= di <= base-1@

floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)
floatToDigits _ 0 = ([0], 0)
floatToDigits base x =
 let
  (f0, e0) = decodeFloat x
  (minExp0, _) = floatRange x
  p = floatDigits x
  b = floatRadix x
  minExp = minExp0 - p -- the real minimum exponent
  -- Haskell requires that f be adjusted so denormalized numbers
  -- will have an impossibly low exponent.  Adjust for this.
  (f, e) =
   let n = minExp - e0 in
   if n > 0 then (f0 `quot` (expt b n), e0+n) else (f0, e0)
  (r, s, mUp, mDn) =
   if e >= 0 then
    let be = expt b e in
    if f == expt b (p-1) then
      (f*be*b*2, 2*b, be*b, be)     -- according to Burger and Dybvig
    else
      (f*be*2, 2, be, be)
   else
    if e > minExp && f == expt b (p-1) then
      (f*b*2, expt b (-e+1)*2, b, 1)
    else
      (f*2, expt b (-e)*2, 1, 1)
  k :: Int
  k =
   let
    k0 :: Int
    k0 =
     if b == 2 && base == 10 then
        -- logBase 10 2 is very slightly larger than 8651/28738
        -- (about 5.3558e-10), so if log x >= 0, the approximation
        -- k1 is too small, hence we add one and need one fixup step less.
        -- If log x < 0, the approximation errs rather on the high side.
        -- That is usually more than compensated for by ignoring the
        -- fractional part of logBase 2 x, but when x is a power of 1/2
        -- or slightly larger and the exponent is a multiple of the
        -- denominator of the rational approximation to logBase 10 2,
        -- k1 is larger than logBase 10 x. If k1 > 1 + logBase 10 x,
        -- we get a leading zero-digit we don't want.
        -- With the approximation 3/10, this happened for
        -- 0.5^1030, 0.5^1040, ..., 0.5^1070 and values close above.
        -- The approximation 8651/28738 guarantees k1 < 1 + logBase 10 x
        -- for IEEE-ish floating point types with exponent fields
        -- <= 17 bits and mantissae of several thousand bits, earlier
        -- convergents to logBase 10 2 would fail for long double.
        -- Using quot instead of div is a little faster and requires
        -- fewer fixup steps for negative lx.
        let lx = p - 1 + e0
            k1 = (lx * 8651) `quot` 28738
        in if lx >= 0 then k1 + 1 else k1
     else
        -- f :: Integer, log :: Float -> Float,
        --               ceiling :: Float -> Int
        ceiling ((log (fromInteger (f+1) :: Float) +
                 fromIntegral e * log (fromInteger b)) /
                   log (fromInteger base))
--WAS:            fromInt e * log (fromInteger b))

    fixup n =
      if n >= 0 then
        if r + mUp <= expt base n * s then n else fixup (n+1)
      else
        if expt base (-n) * (r + mUp) <= s then n else fixup (n+1)
   in
   fixup k0

  gen ds rn sN mUpN mDnN =
   let
    (dn, rn') = (rn * base) `quotRem` sN
    mUpN' = mUpN * base
    mDnN' = mDnN * base
   in
   case (rn' < mDnN', rn' + mUpN' > sN) of
    (True,  False) -> dn : ds
    (False, True)  -> dn+1 : ds
    (True,  True)  -> if rn' * 2 < sN then dn : ds else dn+1 : ds
    (False, False) -> gen (dn:ds) rn' sN mUpN' mDnN'

  rds =
   if k >= 0 then
      gen [] r (s * expt base k) mUp mDn
   else
     let bk = expt base (-k) in
     gen [] (r * bk) s (mUp * bk) (mDn * bk)
 in
 (map fromIntegral (reverse rds), k)
```




# GHC LLVM code generator: Switch to unreachable

The [switch to out of range](https://github.com/ghc/ghc/blob/master/compiler/llvmGen/LlvmCodeGen/CodeGen.hs#L1102) 
code generator switches to the first label. It should be more profitable
to switch to a `unreachable` block. That way, LLVM can take advantage of UB.

# Concurrency in Haskell:

Great link to the GHC wiki that describes the concurrency primitives
"bottom up": https://gitlab.haskell.org/ghc/ghc/wikis/lightweight-concurrency

# Handy list of differential geometry definitions

There are way too many objects in diffgeo, all of them subtly connected.
Here I catalogue all of the ones I have run across:

##### Manifold

A manifold `M` of dimension `n` is a topological space. So, there is a
topological structure `T` on `M`. There is also an `Atlas`, which is a family
of `Chart`s that satisfy some properties.

##### Chart

A chart is a pair `(O ∈  T , cm: O -> R^n)`. The `O` is an open set of the
manifold, and `cm`("chart for "m") is a continous mapping from `O` to `R^n`
under the subspace topology for `U` and the standard topology for `R^n`.

#####  Atlas

An `Atlas` is a collection of `Charts` such that the charts cover the manifold,
and the charts are pairwise compatible. That is, `A = { (U_i, phi_i)}`, such
that `union of U_i = M`, and `phi_j . inverse (phi_i)` is smooth.

##### Differentiable map

`f: M -> N` be a mapping from an `m` dimensional manifold to an `n` dimensional
manifold. Let `frep = cn . f . inverse (cm): R^m -> R^n` where `cm: M -> R^m`
is a chart for `M`, `cn: N -> R^n` is a chart for `N`.`frep` is `f` represented
in local coordinates. If `frep` is smooth for all choices of `cm` and `cn`,
then `f` is a differentiable map from `M` to `N`.

##### Curve: 

Let `I` be an open interval of `R` which includes the point `0`.  A Curve is a
differentiable map `C: (a, b) -> M` where `a < 0 < b`.

##### Function: (I hate this term, I prefer something like Valuation): 

A differentiable mapping from `M` to `R`.


##### Directional derivative of a function `f(m): M -> R` with respect to a curve `c(t): I -> M`, denoted as `c[f]`.

Let `g(t) = (f . c)(t) :: I -c-> M -f-> R = I -> R`.
This this is the value `dg/dt(t0) = (d (f . c) / dt) (0)`.

##### Tangent vector at a point `p`:

On a `m` dimensional manifold `M`, a tangent vector at a point `p` is an
equivalence class of curves that have `c(0) = p`, such that `c1(t) ~ c2(t)` iff
:

2. For a (all) charts `(O, ch)` such that `c1(0) ∈  O`,
 `d/dt (ch . c1: R -> R^m) = d/dt (ch . c2: R -> R^m)`.

 That is, they have equal derivatives.

##### Tangent space(`TpM`):

The set of all tangent vectors at a point `p` forms a vector space `TpM`.
We prove this by creating a bijection from every curve to a vector `R^n`.

Let `(U, ch: U -> R)` be a chart around the point `p`, where `p ∈ U ⊆ M`. Now,
the bijection is defined as:

```
forward: (I -> M) -> R^n
forward(c) = d/dt (c . ch)

reverse: R^n -> (I -> M)
reverse(v)(t) = ch^-1 (tv)
```

##### Cotangent space(`TpM*`): dual space of the tangent space / Space of all linear functions from `TpM` to `R`.

- Associated to every function `f`, there is a cotangent vector, colorfully
  called `df`. The definition is `df: TpM -> R`, `df(c: I -> M) = c[f]`. That is,
  given a curve `c`, we take the directional derivative of the function `f`
  along the curve `c`. We need to prove that this is constant for all vectors
  in the equivalence class and blah.

######  Pushforward `push(f): TpM -> TpN`

Given a curve `c: I -> M`, the pushforward
is the curve `f . c : I -> N`. This extends to the equivalence classes
and provides us a way to move curves in `M` to curves in `N`, and thus
gives us a mapping from the tangent spaces.

This satisfies the identity:

```
push(f)(v)[g] === v[g . f]
```

##### Pullback `pull(f): TpN* -> TpM*`

Given a linear functional `wn : TpN -> R`, the pullback is defined as
` wn . push(f) : TpM -> R`.


This satisfies the identity:

```
(pull wn)(v) === wn (push v)
(pull (wn : TpN->R): TpM->R) (v : TpM) : R  = (wn: TpN->R) (push (v: TpM): TpN) : R
```





## Lazy programs have space leaks, Strict programs have time leaks
Stumbled across this idea while reading some posts on a private discourse.
- Continually adding new thunks without forcing them can lead to a space leak,
  aka the dreaded monadic parsing backtracking problem.

- Continually _running_ new thunks can lead to a "time leak", where we spend
  far too much time running things that should not be run in the first place!

This is an interesting perspective that I've never seen articulated before, and
somehow helps make space leaks feel more... palatable? Before, I had no
analogue to a space leak in the strict world, so I saw them as a pathology. But
with this new perspective, I can see that the strict world's version of a space
leak is a time leak.

## Presburger arithmetic can represent the Collatz Conjecture

An observation I had: the function

```
f(x) = x/2      if (x % 2 == 0)
f(x) = 3x + 1   otherwise
```

is a Presburger function, so by building better approximations to the
transitive closure of a presburger function, one could get better answers
to the Collatz conjecture. Unfortunately, ISL (the integer set library) of today
is not great against the formidable foe.

The code:

```
#include <isl/set.h>
#include <isl/version.h>
#include <isl/map.h>
#include <isl/aff.h>
#include <isl/local_space.h>
#include <isl/constraint.h>
#include <isl/space.h>

int main() {
    isl_ctx *ctx = isl_ctx_alloc();
    const char *s = "{ [x] -> [x / 2] : x % 2 = 0; [x] -> [3 * x + 1] : x % 2 = 1}";

    isl_map *m = isl_map_read_from_str(ctx, s);

    isl_map_dump(m);

    isl_bool b;
    isl_map *p = isl_map_transitive_closure(m, &b);
    printf("exact: %d\n", b);
    printf("map:\n");
    isl_map_dump(p);

}
```

Produces the somewhat disappointing, and yet expected output:

```
$ clang bug.c -lisl -Lisl-0.20/.libs -o bug -I/usr/local/include/
$ ./bug
{ [x] -> [o0] : 2o0 = x or (exists (e0 = floor((1 + x)/2): o0 = 1 + 3x and 2e0 = 1 + x)) }
exact: 0
map:
{ [x] -> [o0] }
```

I find it odd that it is unable to prove _anything_ about the image, even that
it is non-negative, for example. This is an interesting direction in which
to improve the functions `isl_map_power` and `isl_map_transitive_closure`
though.


## Using compactness to argue about the cover in an argument

I've always seen compactness be used by _starting_ with a possibly infinite
coverm and then _filtering it_ into a finite subcover. This finite
subcover is then used for finiteness properties (like summing, min, max, etc.).

I recently ran across a use of compactness when one _starts_ with the set
of _all possible subcovers_, and then argues about why a cover cannot be built
from these subcovers if the set is compact. I found it to be a very cool
use of compactness, which I'll record below:

##### Theorem: 

If a family of compact, countably infinite sets `S_a` have all 
_finite intersections_ non-empty, then the intersection of the family `S_a`
is non-empty.

##### Proof:

Let `S = intersection of S_a`. We know that `S` must be compact since
all the `S_a` are compact, and the intersection of a countably infinite
number of compact sets is compact.

Now, let `S` be empty. Therefore, this means there must be a point `p ∈ P`
such that `p !∈ S_i` for some arbitrary `i`.

##### Cool use of theorem:

We can see that the cantor set is non-empty, since it contains a family
of closed and bounded sets `S1, S2, S3, ...` such that  `S1 ⊇ S2 ⊇ S3 ...`
where each `S_i` is one step of the cantor-ification. We can now see
that the cantor set is non-empty, since:

1. Each finite interesection is non-empty, and will be equal to the set that
   has the highest index in the finite intersection.

2. Each of the sets `Si` are compact since they are closed and bounded subsets of `R`

3. Invoke theorem.


## Japanese Financial Counting system
- [Wikipedia](https://en.wikipedia.org/wiki/Japanese_numerals#Formal_numbers)

Japanese contains a seaparate kanji set called `daiji`, to prevent people
from adding strokes to stuff previously written.

```
#  |Common |Formal
1  |一     |壱
2  |二     |弐
3  |三     |参
```


## Stephen wolfram's live stream
- [Twitch.tv link](https://www.twitch.tv/videos/408653972)


I've taken to watching the live stream when I have some downtime and want
some interesting content. 

The discussions of Wolfram with his group are great, and they bring up
_really_ interesting ideas (like that of cleave being very irregular).

## `Cleave` as a word has some of the most irregular inflections
- cleave
- clove
- cleaved
- clave
- cleft

## McCune's single axiom for group theory
[Single Axioms for Groups and Abelian Groups with Various
Operations](http://ftp.mcs.anl.gov/pub/tech_reports/reports/P270.pdf)
provides a single axiom for groups. This can be useful for some ideas I have
for training groups, where we can use this axiom as the loss function!

## `Word2Vec` C code implements gradient descent really weirdly
I'll be posting snippets of the original source code, along with a 
link to the Github sources. We are interested in exploring the skip-gram
implementation of Word2Vec, with negative sampling, without hiearchical
softmax. I assume basic familiarity with word embeddings and the skip-gram
model.

#### Construction of the sigmoid lookup table
```cpp
// https://github.com/tmikolov/word2vec/blob/master/word2vec.c#L708

expTable = (real *)malloc((EXP_TABLE_SIZE + 1) * sizeof(real));
for (i = 0; i < EXP_TABLE_SIZE; i++) {
  expTable[i] = exp((i / (real)EXP_TABLE_SIZE * 2 - 1) *
                    MAX_EXP);  // Precompute the exp() table
  expTable[i] =
      expTable[i] / (expTable[i] + 1);  // Precompute f(x) = x / (x + 1)
}
```
Here, the code constructs a lookup table which maps `[0...EXP_TABLE_SIZE-1]`
to `[sigmoid(-MAX_EXP)...sigmoid(MAX_EXP)]`. The index `i` first gets mapped
to `(i / EXP_TABLE_SIZE) * 2 - 1`, which sends `0` to `-1` and `EXP_TABLE_SIZE`
to `1`. This is then rescaled by `MAX_EXP`.

#### Layer initialization

- `syn0` is a global variable, initialized with random weights in the range of
`[-0.5...0.5]`. It has dimensions `VOCAB x HIDDEN`.  This layer holds the
hidden representations of word vectors.

```cpp
// https://github.com/imsky/word2vec/blob/master/word2vec.c#L341
a = posix_memalign((void **)&syn0, 128,
               (long long)vocab_size * layer1_size * sizeof(real));
...

// https://github.com/imsky/word2vec/blob/master/word2vec.c#L355
for (a = 0; a < vocab_size; a++)
        for (b = 0; b < layer1_size; b++) {
            next_random = next_random * (unsigned long long)25214903917 + 11;
            syn0[a * layer1_size + b] =
                (((next_random & 0xFFFF) / (real)65536) - 0.5) / layer1_size;
        }
```


- `syn1neg` is a global variable that is zero-initialized. It has dimensions
`VOCAB x HIDDEN`. This layer also holds hidden representations of word vectors,
_when they are used as a negative sample_.

```cpp
// https://github.com/imsky/word2vec/blob/master/word2vec.c#L350
a = posix_memalign((void **)&syn1neg, 128,
                   (long long)vocab_size * layer1_size * sizeof(real));
...
for (a = 0; a < vocab_size; a++)
    for (b = 0; b < layer1_size; b++) syn1neg[a * layer1_size + b] = 0;
```

- `neu1e` is a temporary per-thread buffer (Remember that the `word2vec` C code
use CPU threads for parallelism) which is zero initialized. It has dimensions
`1 x HIDDEN`.

```cpp
// https://github.com/imsky/word2vec/blob/master/word2vec.c#L370
real *neu1e = (real *)calloc(layer1_size, sizeof(real));
```

#### Backpropogation

Throughout `word2vec`, no 2D arrays are used. Indexing of the form
`arr[word][ix]` is manually written as `arr[word * layer1_size + ix]`. So, I
will call `word * layer1_size` as the "base address", and `ix` as the "offset
of the array index expression henceforth.

Here, `l1` is the base address of the word at the center of window (the focus
word).  `l2` is the base address of either the word that is negative sampled
from the corpus, or the word that is a positive sample from within the context
window.

`label` tells us whether the sample is a positive or a negative sample. 
`label = 1` for positive samples, and `label = 0` for negative samples.

```cpp
// zero initialize neu1e
// https://github.com/imsky/word2vec/blob/master/word2vec.c#L419
for (c = 0; c < layer1_size; c++) neu1e[c] = 0;
...
// loop through each negative sample
// https://github.com/imsky/word2vec/blob/master/word2vec.c#L508
if (negative > 0)  for (d = 0; d < negative + 1; d++) {
  ...
  // https://github.com/imsky/word2vec/blob/master/word2vec.c#L521
  // take the dot product: f=  syn0[focus] . syn1neg[context]
  for (c = 0; c < layer1_size; c++) f += syn0[c + l1] * syn1neg[c + l2];
  
  // compute: g = (label - sigmoid(2f - 1)) * alpha
  // g is computed using lookups into a lookup table and clamping for
  // efficiency.
  if (f > MAX_EXP) g = (label - 1) * alpha;
  else if (f < -MAX_EXP) g = (label - 0) * alpha;
  else
  g = (label - expTable[(int)((f + MAX_EXP) *
                              (EXP_TABLE_SIZE /
                               MAX_EXP / 2))]) * alpha;
  // Now that we have computed the gradient:
  // `g = (label - output) * learningrate`,
  // we need to perform backprop. This is where the code gets weird.

  for (c = 0; c < layer1_size; c++) neu1e[c] += g * syn1neg[c + l2];
  for (c = 0; c < layer1_size; c++) syn1neg[c + l2] += g * syn0[c + l1];
  } // end loop through negative samples
// Learn weights input -> hidden
for (c = 0; c < layer1_size; c++) syn0[c + l1] += neu1e[c];
```

- We have _two_ vectors for each word, one called `syn0[l1 + _]` and 
the other `syn1neg[l2 + _]`. The `syn1neg` word embedding is used whenever
a word is used a negative sample, and is not used anywhere else. Also,
the `syn1neg` vector is zero initialized, while the `syn0` vectors are
randomly initialized.

- The values we backprop with `g * syn1neg[l2 + _]`, `g * syn0[l1 + _]` are
  _not_ the correct gradients of the error term! The derivative of a sigmoid
  is `dsigmoid(x)/dx = sigmoid(x) [1 - sigmoid(x)]`. The `[1 - sigmoid(x)]`
  is nowhere to be seen, let alone the fact that we are using 
  `sigmoid(2x - 1)` and not regular sigmoid. Very weird.

- We hold the value of `syn0` constant throughout all the negative samples,
which was not mentioned in any tutorial I've read.

The paper does not mentioned these implementation details, and neither
does _any blog post that I've read_. I don't understand what's going on,
and I plan on updating this section when I understand this better.


## Arthur Whitney: dense code
- Guy who wrote a bunch of APL dialects, write code in an eclectic style
  that has very little whitespace and single letter variable names.
- Believes that this allows him to hold the entire program in his head.
- Seems legit from my limited experience with APL, haskell one-liners.
- [The b programming language](http://kparc.com/b/readme.txt). It's quite 
  awesome to read the sources. For example, [`a.c`](http://kparc.com/b/a.c)
## How does one work with arrays in a linear language?

Given an array of qubits `xs: Qubit[]`, I want to switch to little endian.
Due to no-cloning, I can't copy them! I suppose I can use recursion to build
up a new "list". But this is not the efficient array version we know and love
and want. 

The code that I want to work but does not:
```csharp
function switchEndian(xs: Qubit[]): Unit {
    for(i in 0..Length(xs) - 1) {
        Qubit q = xs[i]; // boom, this does not work!
        xs[i] = xs[Length(xs) - 1 - i]
        xs[Length(xs) - 1 - i] = q;
    }
}
```

On the other hand, what _does work_ is to setup a quantum circuit that
performs this flipping, since it's a permutation matrix at the end of
the day. But this is very slow, since it needs to simulate the "quantumness"
of the solution, since it takes `2^n` basis vectors for `n` qubits. 

However, the usual recursion based solution works:
```csharp
function switchEndian(xs: Qubit[]): Qubit[] {
    if(Length(xs) == 1) {
        return xs;
    } else {
        switchEndian(xs[1..(Length(xs) - 1)] + xs[0]
    }
}
```

This is of course, suboptimal.

I find it interesting that in the linear types world, often the "pure" solution
is _forced_ since mutation very often involves temporaries / copying!

(I'm solving assignments in [qsharp](https://docs.microsoft.com/en-us/quantum/)
for my course in college)

## How Linear optimisation is the same as Linear feasibility checking
Core building block of effecively using the ellipsoid algorithm.

- If we posess a way to check if a point `p ∈ P` where `P` is a polytope, we
  can use this to solve optimisation problems.
- Given the optimisation problem maximise `c^Tx` subject to `Ax = b`, we can
  construct a new _non-emptiness_ problem. This allows us to convert optimisation
  into _feasibility_.
- The new problem is `Ax = b, A^Ty = c, c^Tx = b^T y`. Note that by duality,
  a point in this new polyhedra will _be an optimal solution to the above linear program_.
  We are forcing `c^Tx = b^Ty`, which will be the optimal solution, since the
  solution where the primal and dual agree is the optimal solution by strong
  duality.
- This way, we have converted a _linear programming_ problem into a 
  _check if this polytope is empty_ problem!

## Quantum computation without complex numbers
I recently learnt that the Toeffili and Hadamard gates are universal for
quantum computation. The description of these gates involve no complex numbers.
So, we can write any quantum circuit in a "complex number free" form. The caveat
is that we may very well have _input qubits_ that require complex numbers.

Even so, a large number (all?) of the basic algorithms shown in Nielsen and
Chaung can be encoded in an entirely complex-number free fashion.

I don't really understand the ramifications of this, since I had the intuition
that the power of quantum computation comes from the ability to express
complex phases along with superposition (tensoring). However, I now have
to remove the power from going from R to C in many cases. This is definitely
something to ponder.


## Linguistic fun fact: Comparative Illusion

I steal from wikipedia:

> Comparative Illusion, which is a grammatical illusion where certain
> sentences seem grammatically correct when you read them, but upon further
> reflection actually make no sense. 

For example: "More people have been to Berlin than I have."

# Long-form posts:
## Reading
- [2018 reading](content/blog/stuff-i-learnt-this-year-2018.md)
- [2017 reading](content/blog/papers-I-read-and-loved-in-2017.md)

## Haskell
- [Reading the `structs` library](content/blog/reading-kmett-structs.md)
- [Reading the `machines` library (WIP)](content/blog/machines/reading-kmett-machines.md)
- [Explaining laziness (WIP)](content/blog/laziness-for-c-programmers.md)
- [Explaining STG(WIP)](stg-explained.md)

## Simplexhc (STG -> LLVM compiler) progress
- [proc points suck / making GHC an order of magnitude faster](content/blog/ghc-micro-optimisations-or-why-proc-points-suck.md)
    Note: this renders better on the website. I've put it up here,
    but I need to migrate the images and plots to be static.

- [dec 2017](this-month-in-simplexhc-dec-2017.md)
- [oct 29 2017](this-week-in-simpexhc-oct-29-2017.md)
- [july 2017](this-week-in-simplexhc-07-2017.md)
- [july 6th 2017](this-week-in-simplexhc-2017-07-06.md)
- [announcement](content/blog/announcing-simplexhc.md)

## GSoC (2015)
- [proposal](content/blog/gsoc-vispy.md)
- [week 1](content/blog/gsoc-vispy-week-1-and-2.md)
- [week 3 and 4](content/blog/gsoc-vispy-week-3-and-4.md)
- [week 5](content/blog/gsoc-vispy-week-5.md)
- [week 6](content/blog/gsoc-vispy-week-6.md)
- [week 7](content/blog/gsoc-vispy-week-7.md)
- [final report](content/blog/gsoc-vispy-report-6.md)
