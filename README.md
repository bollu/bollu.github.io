<script type="text/x-mathjax-config">
  MathJax.Hub.Config({
    tex2jax: {
      inlineMath: [ ['$','$'], ["\\(","\\)"] ],
      processEscapes: true
    }
  });
</script>
<script
  type="text/javascript"
  charset="utf-8"
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
>
</script>
<script
  type="text/javascript"
  charset="utf-8"
  src="https://vincenttam.github.io/javascripts/MathJaxLocal.js"
>
</script>


- [My github](http://github.com/bollu)
- [My math.se profile](https://math.stackexchange.com/users/261373/siddharth-bhat)
- email ID: rot13(`fvqqh.qehvq@tznvy.pbz`)

# Ideas I stumble onto

# Computing equivalent gate sets using grobner bases

We are given the grammar for a language `L`:

```
E = T +_mod8 T | T
T = V | V ^ V | V ^ V ^ V
V = 'a1' | 'a2' | ...
```


where `+_mod8` is addition modulo 8, and `^` is XOR. We are guaranteed that
our variables `a_i ∈ {0, 1}`.  This language is equipped with the obvious
evaluation rules, corresponding to those of arithmetic.


We are then given the input expression `(a0 ^ a1 ^ a2 ^ a3)`. We wish
to find an equivalent expression in terms of the above language `L`.


Naturally, the first idea that I thought was that of employing a grobner basis,
since they essentially embody rewrite rules modulo polynomial equalities, which
is precisely our setting here.

##### What the hell is Grobner Basis?

The nutshell is that a grobner basis is a way to construct rewrite rules which
also understand arithmetic (I learnt this viewpoint from the book "Term
rewriting and all that". Fantastic book in general). Expanding on the
nutshell, assume we have a term rewriting system:

```
A -> -1*B -- (1)
C -> B^2  -- (2)
```

over an alphabet `{A, B, C}`.

Now, given the string `C + AB`, we wish to find out if it can be rewritten to 
`0` or not. Let's try to substitute and see what happens:

```
C + AB -2-> B^2 + AB -1-> B^2 + (-1*B)B
```

At this point, we're stuck! we don't have rewrite rules to allow us to
rewrite `(-1*B)B` into `-B^2`. Indeed, creating such a list would be 
infinitely long. But if we are willing to accept that we somehow have
the rewrite rules that correspond to polynomial arithmetic, where we view
`A, B, C` as variables, then we _can_ rewrite the above string to 0:

```
B^2 + (-1*B)B -> B^2 - B^2 -> 0
```

A Grobner basis is the algorithmic / mathematical machine that allows us
to perform this kind of substitution.

In this example, this might appear stupid: what is so special? We simply
substituted variables and arrived at `0` by using arithmetic. What's
so complicated about that? To understand why this is not always so easy,
let's consider a pathological, specially constructed example


##### A complicated example that shatters dreams

Here's the pathological example:

```
A -> 1     -- (1)
AB -> -B^2 -- (2)
```

And we consider the string `S = AB + B^2`.  If we blindly apply the
first rule, we arrive at:

```
S = AB + B^2 -1> 1B + B^2 = B + B^2 (STUCK)
```

However, if we apply `(2)` and then `(1)`:

```
S = AB + B^2 -2> -B^2 + B^2 -> 0
```

This tells us that we _can't just apply the rewrite rules willy-nilly_. 
It's sensitive to the _order_ of the rewrites! That is, the rewrite system
is not [confluent](https://en.wikipedia.org/wiki/Confluence_(abstract_rewriting).


The grobner basis is a function from rewrite systems to rewrite systems.
When given a rewrite system `R`, it produces a _new_ rewrite system `R'`
that _is confluent_. So, we can apply the rewrite rules of `R'` in any order,
and we guaranteed that _we will only get a 0 from `R'` if and only if
we could have gotten a `0` from `R`_ for all strings.

We can then go on to phrase this whole rewriting setup in the language of
ideals from ring theory, and that is the language in which it is most 
often described. [I've gone into more depth on that perspective here: "What is a grobner basis? polynomial
factorization as rewrite systems"](What-is-a-Grobner basis-polynomial-factorization-as-rewrite-systems).

Now that we have a handle on what a grobner basis is, let's go on to solve
the original problem:


##### An explanation through a slightly simpler problem

I'll first demonstrate the idea of how to solve the original problem
by solving a slightly simpler problem:

> Rewrite `a^b^c` in terms of `a^b`, `b^c`, `c^a` and the same `+_mod8` instruction
> set as the original problem. The only difference is that we do _not_ have `T -> V ^ V ^ V`.


The idea is to construct the polynomial ring over `Z/8Z` (integers modulo 8) with 
variables as `a, b, c, axorb, bxorc, axorc`. Now, we know that `a^b = a + b - 2ab`. So, 
we setup rewrite rules such that `a + b - 2ab -> axorb`, `b + c - 2bc -> bxorb`, 
`c + a - 2ca -> cxora`.


We construct the _polynomial_ `f(a, b, c) = a^b^c`, which
has been written in terms of addition and multiplication, defined
as `f_orig(a, b, c) = 4*a*b*c - 2*a*b - 2*a*c - 2*b*c + a + b + c`. We then
rewrite `f_orig` with respect to our rewrite rules. Hopefully, the rewrite
rules should give us a clean expression in terms of one variable and 
two-variable `xor`s. There is the danger that we may have some term
such as `a * bxorc`, and we do get such a term (`2*b*axorc`) in this case,
but it does not appear in the _original_ problem.



```py
# Create ring with variables a, b, c, axorb, bxorc, axorc
R = IntegerModRing(8)['a, b, c, axorb, bxorc, axorc']
(a, b, c, axorb, bxorc, axorc) = R.gens()


# xor in terms of polynomials
def xor2(x, y): return x + y - 2*x*y

# define the ideal which allows us to rewrite xor2(a, b) -> axorb, and so on
# we also add the relation a^2 - a = 0 => a = 0 or a = 1 in case this helps
# the solver.
I = ideal((axorb - xor2(a, b), bxorc - xor2(b, c), axorc - xor2(a, c), a*a-a, b*b-b, c*c-c))

# the polynomial representing a^b^c we wish to reduce
f_orig = xor3(a, b, c)

# we take the groebner basis of the ring to reduce the polynomial f.
IG = I.groebner_basis()

# we reduce a^b^c with respect to the groebner basis.
f_reduced = f_orig.reduce(IG)

print("value of a^b^c:\n\t%s\n\treduced: %s" % (f_orig, f_reduced))

def evalxor2(f):
    for (i, j, k) in [(i, j, k) for i in [0, 1] for j in [0, 1] for k in [0, 1]]:
      ref = i^^j^^k
      eval = f.substitute(a=i, b=j, c=k, axorb=i^^j, bxorc=j^^k, axorc=i^^k)
      print("%s^%s^%s: ref(%s) =?= f(%s): %s" % 
        (i, j, k, ref, eval, ref == eval))

  
print("evaulating original f for sanity check:")
evalxor2(f_orig)


print("evaulating reduced f:")
evalxor2(f_reduced)
```

Running the code gives us the reduced polynomial `-2*b*axorc + b + axorc`
which unfortunately contains a term that is `b * axorc`. So, this approach
does not work, and I was informed by my friend that she is unaware
of a solution to this problem (writing `a^b^c` in terms of smaller xors and
sums).


The full code output is:

```
value of a^b^c:
	4*a*b*c - 2*a*b - 2*a*c - 2*b*c + a + b + c
	reduced: -2*b*axorc + b + axorc
evaulating original f for sanity check:
0^0^0: ref(0) =?= f(0): True
0^0^1: ref(1) =?= f(1): True
0^1^0: ref(1) =?= f(1): True
0^1^1: ref(0) =?= f(0): True
1^0^0: ref(1) =?= f(1): True
1^0^1: ref(0) =?= f(0): True
1^1^0: ref(0) =?= f(0): True
1^1^1: ref(1) =?= f(1): True
evaulating reduced f:
0^0^0: ref(0) =?= f(0): True
0^0^1: ref(1) =?= f(1): True
0^1^0: ref(1) =?= f(1): True
0^1^1: ref(0) =?= f(0): True
1^0^0: ref(1) =?= f(1): True
1^0^1: ref(0) =?= f(0): True
1^1^0: ref(0) =?= f(0): True
1^1^1: ref(1) =?= f(1): True
```

That is, both the original polynomial and the reduced polynomial match
the expected results. But the reduced polynomial is not in our language `L`,
since it has a term that is a _product_ of `b` with `axorc`. 


##### Tacking the original problem.

We try the exact same approach to the original problem of expressing
`a ^ b ^ c ^ d`. We find that the reduced polynomial is 

```
-a - b + c + 3*d - 3*axorb - axorc
+ axord - bxorc + bxord + 3*cxord 
- 3*axorbxorc - axorbxord 
+ axorcxord + bxorcxord
```

which happily has no products between terms! It also passes our sanity
check, so we've now found the answer.

The full output is:
```
value of a^b^c^d:
	4*a*b*c + 4*a*b*d + 4*a*c*d + 4*b*c*d - 2*a*b - 2*a*c - 2*b*c - 2*a*d - 2*b*d - 2*c*d + a + b + c + d
	reduced: -a - b + c + 3*d - 3*axorb - axorc + axord - bxorc + bxord + 3*cxord - 3*axorbxorc - axorbxord + axorcxord + bxorcxord
evaluating original a^b^c^d
0^0^0^0: ref(0) =?= f(0): True
0^0^0^1: ref(1) =?= f(1): True
0^0^1^0: ref(1) =?= f(1): True
0^0^1^1: ref(0) =?= f(0): True
0^1^0^0: ref(1) =?= f(1): True
0^1^0^1: ref(0) =?= f(0): True
0^1^1^0: ref(0) =?= f(0): True
0^1^1^1: ref(1) =?= f(1): True
1^0^0^0: ref(1) =?= f(1): True
1^0^0^1: ref(0) =?= f(0): True
1^0^1^0: ref(0) =?= f(0): True
1^0^1^1: ref(1) =?= f(1): True
1^1^0^0: ref(0) =?= f(0): True
1^1^0^1: ref(1) =?= f(1): True
1^1^1^0: ref(1) =?= f(1): True
1^1^1^1: ref(0) =?= f(0): True
evaluating reduced a^b^c^d
0^0^0^0: ref(0) =?= f(0): True
0^0^0^1: ref(1) =?= f(1): True
0^0^1^0: ref(1) =?= f(1): True
0^0^1^1: ref(0) =?= f(0): True
0^1^0^0: ref(1) =?= f(1): True
0^1^0^1: ref(0) =?= f(0): True
0^1^1^0: ref(0) =?= f(0): True
0^1^1^1: ref(1) =?= f(1): True
1^0^0^0: ref(1) =?= f(1): True
1^0^0^1: ref(0) =?= f(0): True
1^0^1^0: ref(0) =?= f(0): True
1^0^1^1: ref(1) =?= f(1): True
1^1^0^0: ref(0) =?= f(0): True
1^1^0^1: ref(1) =?= f(1): True
1^1^1^0: ref(1) =?= f(1): True
1^1^1^1: ref(0) =?= f(0): True
```

##### code for `a^b^c^d` reduction:


```py
def xor3(x, y, z): return xor2(x, xor2(y, z))

R = IntegerModRing(8)['a, b, c, d, axorb, axorc, axord, bxorc, bxord, cxord, axorbxorc, axorbxord, axorcxord, bxorcxord']

(a, b, c, d, axorb, axorc, axord, bxorc, bxord, cxord, axorbxorc, axorbxord, axorcxord, bxorcxord) = R.gens()
I = ideal((axorb - xor2(a, b),
           axorc - xor2(a, c),
           axord - xor2(a, d),
           bxorc - xor2(b, c),
           bxord - xor2(b, d),
           cxord - xor2(c, d),
           axorbxorc - xor3(a, b, c),
           axorbxord - xor3(a, b, d),
           axorcxord - xor3(a, c, d),
           bxorcxord - xor3(b, c, d),
           a*a-a,
           b*b-b,
           c*c-c,
           d*d-d
           ))
IG = I.groebner_basis()
f_orig = (xor2(a, xor2(b, xor2(c, d))))
f_reduced = f_orig.reduce(IG)
print("value of a^b^c^d:\n\t%s\n\treduced: %s" % (f_orig, f_reduced))

def evalxor3(f):
    for (i, j, k, l) in [(i, j, k, l) for i in [0, 1] for j in [0, 1] for k in [0, 1] for l in [0, 1]]:
      ref = i^^j^^k^^l
      eval = f.substitute(a=i, b=j, c=k, d=l, axorb=i^^j, axorc=i^^k, axord=i^^l, bxorc=j^^k, bxord=j^^l, cxord=k^^l, axorbxorc=i^^j^^k, axorbxord=i^^j^^l,
                          axorcxord=i^^k^^l, bxorcxord=j^^k^^l)
      print("%s^%s^%s^%s: ref(%s) =?= f(%s): %s" % 
        (i, j, k, l, ref, eval, ref == eval))

print("evaluating original a^b^c^d")
evalxor3(f_orig)


print("evaluating reduced a^b^c^d")
evalxor3(f_reduced)
```

##### Closing thoughts

This was a really fun exercise: Around a hundred lines of code illuminates
the use of machinery such as grobner basis for solving real-world problems!
I really enjoyed hacking this up and getting nerd sniped.


# The janus programming language: Time reversible computation

- [Wiki link](https://en.wikipedia.org/wiki/Janus_(time-reversible_computing_programming_language)
- [Original letter to Landlauer](http://tetsuo.jp/ref/janus.pdf)

# A = B: A book about proofs of combinatorial closed forms


The book explains algorithms on solving closed forms for combinatorial
recurrences, by means of [Zeilberger's algorithm](http://mathworld.wolfram.com/ZeilbergersAlgorithm.html).

The book is written by Zeilberger himself, and supposedy also teaches one Maple.
I'd like to learn the algorithm, since it might be useful eventually for
Groebner basis / loop analysis shenanigans I like to play as part of
my work on compilers.

- [Download link here](https://www.math.upenn.edu/~wilf/AeqB.pdf)

# [Generating `k` bitsets of a given length `n`](#generating-k-bitsets-of-a-given-length-n):

The problem is to generate all bitvectors of length `n` that have `k` bits
set. For example, generate all bitvectors of length `5` that have `3` bits
set.

I know that an algorithm exists in Hacker's delight, but I've been too sick
to crack open a book, so I decided to discover the algorithm myself. The one
I came up with relies on looking at the numbers moving at a certain velocity,
and them colliding with each other. For example, let us try to generate all
`5C3` combinations of bits.


We start wih:
```
#1           count of position
a b c d e    positions
1 1 1 0 0    bitset 
< - - - -    velocity
```

Where the `<` represents that the `1` at position `a` is moving leftwards.
Our arena is _circular_, so the leftmost `1` can wrap around to the right.
This leads to the next state

```
#2
a b c d e
0 1 1 0 1
- - - - <
```

We continue moving left peacefully.

```
#3
a b c d e
0 1 1 1 0
- - - < -
```

whoops, we have now collided with a block of `1`s. Not to worry, we simply
transfer our velocity by way of collision, from the `1` at `d` to the `1` at `b`.

I denote the transfer as follows:
```
#3
a b c d e
0 1 1 1 0  original state
- - - < -
- < < < -  transfer of velocity
- < - - -  final state after transfer of velocity
```    

The `1` at `b` proceeds along its merry way with the given velocity

```
#4
a b c d e
1 0 1 1 0
< - - - -
```

Once again, it wraps around, and suffers a collision

```
#5
a b c d e
0 0 1 1 1
- - - - - < (collision, transfer)
- - < < < transfer of velocity
- - < - - final state after transfer of velocity
```

This continues:

```
0 1 0 1 1  #6
- < - - -
1 0 0 1 1  #7
< - - - - (collision, transfer velocity)
< - - < <
- - - < -
1 0 1 0 1 #8
- - < - -
1 1 0 0 1 #9
- < - - - (colision, transfer velocity
< < - - <
- - - - <
1 1 0 1 0 #10
- - - < - 
1 1 1 0 0 #11: wrap around to initial state
```

I don't have a proof of correctness, but I have an intuition that this
should generate all states. Does anyone have a proof?

_EDIT:_ [this algorithm does not work](https://math.stackexchange.com/questions/3398241/correctness-proof-for-algorithm-to-generate-k-bitsets-of-n-bits-nck),
since it will keep clusters of $k-1$ bits next to each other, when a 
bit hits a cluster of $k - 1$ bits.  For completeness, I'm going to draft out
the usual algorithm in full:

### Usual Algorithm

Let's consider the same example of `5C3`:

```
000111
```


# Bondi k-calculus

- [Link here](https://en.wikipedia.org/wiki/Bondi_k-calculus)

An alternative formalism to derive special relativity geometrically,
resting purely on hypotehses about the way light travels.

However, I've not been able to prove the correctness of the assumptions made,
by using coordinate geometry. I suspect this is because I will need to use
hyperbolic geometry for the "side lengths" to work out.


Indeed, I found another source, called as [The k-calculus fiddle](http://bearsoft.co.uk/Kcalc.html)
which attempts to discredit k-calculus. The author of the above blog writes at
the end:

> In asking Ray D'Inverno's permission to use his book as the example of
> k-calculus, he was kind enough to point out that the arguments I have given
> are invalid. Chapter 2 of his book should be read through to the end and then
> reread in the light of the fact that the geometry of space and time is
> Minkowskian. Euclidean geometry should not be used in interpreting the
> diagrams because their geometry is Minkowskian.

which seems to imply that we need to use hyperbolic geometry for this.

# Topology as an object telling us what zero-locus is closed:

- [Idea from this amazing post on `math.se`](https://math.stackexchange.com/questions/53852/is-there-a-way-of-working-with-the-zariski-topology-in-terms-of-convergence-limi)

# Vivado toolchain craziness 

I found this file as I was cleaning up some old code, for a project to implement
a [fast K/V store on an FPGA](https://github.com/AakashKT/CuckooHashingHLS),
so I thought I should put this up for anyone else who stumbles on the
same frustrations / errors. I'm not touching this particular toolchain again
with a 10-foot pole till the tools stabilize by *a lot*.

##### Vivado HLS issues


- Unable to create BRAM for fields such as `bool`, `int16`. The data buses
will be `8/16` bits long, with error:

```
[BD 41-241] Message from IP propagation TCL of /blk_mem_gen_7: set_property
error: Validation failed for parameter 'Write Width A(Write_Width_A)' for BD
Cell 'blk_mem_gen_7'. Value '8' is out of the range (32,1024) Customization
errors found on 'blk_mem_gen_7'. Restoring to previous valid configuration.
```

- I had an array of structs:

```cpp
struct S {
    bool b;
    int16 x;
    int16 y;
}
```

This gets generated as 3 ports for memory, of widths `1`, `16`, `16`. Ideally,
I wanted *one* port, of width `16+16+1=33`, for each struct value.
However, what was generated were three ports of widths `1`, `16`, and `16`
which I cannot connect to BRAM.

- `data_pack` allows us to create one port of width `16+16+1=33`

- Shared function names allocated on BRAM causes errors in synthesis:
```cpp
struct Foo {...};
void f (Foo conflict) {
    #pragma HLS interface bram port=conflict
}

void g (Foo conflict) {
    #pragma HLS interface bram port=conflict
}
```


- Enums causes compile failure in RTL generation  (commit `3c0d619039cff7a7abb61268e6c8bc6d250d8730`)
- `ap_int` causes compile failurre in RTL generation (commit `3c0d619039cff7a7abb61268e6c8bc6d250d8730`)
- `x % m` where `m != 2^k` is very expensive -- there must be faster encodings of modulus?
- How to share code between HLS and vivado SDK? I often wanted to share constant values between
  my HLS code and my Zynq code.
- Can't understand why array of structs that were packed does not get deserialized correctly. I had to manually
  pack a struct into a `uint32`. For whatever reason, having a `#pragma pack` did something to the representation of the struct
  as far as I can tell, and I couldn't treat the memory as just a raw `struct *` on the other side:

```cpp
// HLS side
struct Vec2  { int x; int y};
void f(Vec2 points[NUM_POINTS]) {
	#pragma HLS DATA_PACK variable=points
    #pragma HLS INTERFACE bram port=points

    points[0] = {2, 3};
}

// Host side
Vec2 *points = (Vec2 *)(0xPOINTER_LOCATION_FROM_VIVADO);

int main() {
    // points[0] will *not* be {2, 3}!
}
```

- If I change my IP, there is no way to preserve the current connections in the
  GUI why just updating the "changed connections". I'm forced to remove the IP
  and add it again (no, the Refresh IP button does not work).
- On generating a new bitstream from Vivado, Vivado SDK tries to reload the config,
fails at the reloading (thinks `xil_print.h` doesn't exist), and then fails to compile code.
Options are to either restart Vivado SDK, or refresh `xil_print.h`.


- It is entirely unclear what to version control in a vivado project, unless one
has an omniscient view of the _entire toolchain_. I resorted to `git add` ing 
everything, but this is a terrible strategy in so many ways.


#### SDAccel bugs

**[link to tutorial we were following](https://www.xilinx.com/support/documentation/sw_manuals/xilinx2017_1/ug1028-sdsoc-intro-tutorial.pdf)**
- The executable is named `.exe` while it's actually an ELF executable (The SDAccel tutorials say it is called as `.elf`)
- the board is supposed to automatically boot into linux, which it does not. One is expected to call `bootd` manually (for "boot default") so it boots ito linux. (The SDAccel tutorials say it automatically boots into it)
- At this point, the SD card is unreadable. It took a bunch of time to figure out that the SD card needs to be mounted by us, and has the mount name `/dev/mmcblk0p1`. (The SDAccel tutorials say that it should be automatically mounted)
- At this point, we are unable to run `hashing.elf`. It dies with a truly bizarre error: `hashing.elf: command not found`. This is almost un-googleable, due to the fact that the same problem occurs when people don't have the correct file name.
- I rewrote `ls` with `hashing.elf` to see what would happen, because I conjectured that the shell was able to run `coreutils`. 
- This dies with a different error `ls: core not found`. I'd luckily seen this during my android days, and knew this was from busybox.
- This led me to google "busybox unable to execute executable", which led me to this [StackOverflow link](https://stackoverflow.com/questions/1562071/how-can-i-find-which-elf-dependency-is-not-fulfilled) that clued me into the fact that the ELF interpreter is missing.
- When I discovered this, I wound up trying to understand how to get the right ELF interpreter. `readelf -l <exe name>` dumps out `[Requesting program interpreter: /lib/ld-linux-armhf.so.3]`. So, I bravely copied: `cp /lib/ld-linux.so.3 /lib/ld-linux-armhf.so.3`.
- Stuff is *still* broken, but I now get *useful* error messages:
```
zynq> /hashing.elf 
/hashing.elf: error while loading shared libraries: libxilinxopencl.so: cannot open shared object file: No such file or directory
```
At this point, clearly we have some linker issues (why does `xocc` not correctly statically link? What's up with it? Why does it expect it to be able to load a shared library? **WTF is happening**). do note that this is _not_ the way the process
is supposed to go according to the tutorial!  
- Of course, there's no static library version of `libxilinxopencl.so`, so that's a dead end. I'm completely unsure if the tutorial even makes sense. 
- This entire chain of debugging is full of luck.

- [Link talking about generating `BOOT` file](https://www.xilinx.com/html_docs/xilinx2018_2/sdsoc_doc/compiling-and-running-applications-on-arm-processor-hjy1504034381720.html)


At some point, I gave up on the entire enterprise.

# What is a Grobner basis -  polynomial factorization as rewrite systems

##### A motivating example

The question a Grobner basis allows us to answer is this: can the polynomial
$p(x, y) = xy^2 + y$ be factorized in terms of $a(x, y) = xy + 1, b(x, y) = y^2 - 1$,
such that $p(x, y) = f(x, y) a(x, y) + g(x, y) b(x, y)$ for some _arbitrary_ polynomials
$f(x, y), g(x, y) \in R[x, y]$.

One might imagine, "well, I'll divide and see what happens!" Now, there are two
routes to go down:

- $xy^2 + y = y(xy + 1) = y a(x, y) + 0 b(x, y)$. Well, problem solved?
- $xy^2 + y = xy^2 - x + x + y = x (y^2 - 1) + x + y = x b(x, y) + (x + y)$. Now what? we're stuck, and we can't apply `a(x, y)`!

So, clearly, the _order_ in which we perform of factorization / division starts
to matter! Ideally, we want an algorithm which is _not sensitive_ to the order
in which we choose to apply these changes. $x^2 + 1$.


##### The rewrite rule perspective


An alternative viewpoint of asking "can this be factorized", is to ask
"can we look at the factorization as a rewrite rule"? For this perspective,
notice that "factorizing" in terms of $xy + 1$ is the same as being
able to set $xy = -1$, and then have the polynomial collapse to zero.
(For the more algebraic minded, this relates to the fact that $R[x] / p(x) \sim R(\text{roots of p})$).
The intuition behind this is that when we "divide by $xy + 1$", really what
we are doing is we are setting $xy + 1 = 0$, and then seeing what remains. But
$xy + 1 = 0 \iff xy = -1$. Thus, we can look at the original question as:

How can we apply the rewrite rules $xy \rightarrow -1$, $y^2 \righarrow 1$,
along with the regular rewrite rules of polynomial arithmetic to the polynomial
$p(x, y) = xy^2 + y$, such that we end with the value $0$?

Our two derivations above correspond to the application of the rules:

- $xy^2 + y \xrightarrow{xy = -1} -y + y = 0$
- $xy^2 + y \xrightarrow{y^2 = 1} x + y \nrightarrow \text{stuck!}$

That is, our [rewrite rules are not confluent](https://en.wikipedia.org/wiki/Confluence_(abstract_rewriting))

The grobner basis is a mathematical object, which is a  _a confluent set of rewrite rules_
for the above problem. That is, it's a set of polynomials which manage to find
the rewrite $p(x, y) \xrightarrow{\star} 0$, regardless of the order in which
we apply them. It's also _correct_, in that it only rewrites to $0$ if the
original system had _some way_ to rewrite to $0$.

###### The buchberger's algorithm

We need to identify 
[critical pairs](https://en.wikipedia.org/wiki/Critical_pair_(logic)), 
which in this setting are called as S-polynomials.

Let $f_i = H(f_i) + R(f_i)$ and $f_j = H(f_j) + R(f_j)$. Let $m = lcm(H(f_i), H(f_j))$,
and let $m_i, m_j$ be monomials such that $m_i \cdot H(f_i) = m = m_j \cdot H(f_j)$.
The S-polynomial induced by $f_i, f_j$ is defined as $S(f_i, f_j) = m_i f_i - m_i f_j$.


##### References
- [The term rewriting perspective is from the book "term rewriting and all that"](https://www21.in.tum.de/~nipkow/TRaAT/)
- [Sympy has excellent reading material on grobner basis](https://mattpap.github.io/masters-thesis/html/src/groebner.html)


# Lie bracket versus torsion


![torsion-vs-parallel-transport](static/lie-bracket-versus-torsion.png)

This picture _finally_ made the difference between these two things clear.
The lie bracket moves along the _flow_, while the torsion moves along
_parallel transport_. 

This is why the sides of the parallelogram that measure torsion form,
well, a parallelogram: we set them up using parallel transport.

On the other hand, the lie bracket measures the actual failure of the parallelogram
from being formed.

# [Blog post: Weekend paper replication of STOKE, the stochastic superoptimizer](https://github.com/bollu/blaze/blob/master/notebooks/tutorial.ipynb)

Click the title to go to the post. We replicate the `STOKE` paper in haskell,
to implement a superoptimiser based on MCMC methods.

# Collapsing `BlockId`, `Label`, `Unique`: 

We have this hiearchy of `BlockId`, `Label`, and `Unique` that can be
collapsed. 


# Spatial partitioning data structures in molecular dynamics

[Cell lists](https://en.wikipedia.org/wiki/Cell_lists) and
[Verlet lists](https://en.wikipedia.org/wiki/Verlet_list)

appear to be version of spatial hierarchical data structures for fast
interaction computation. Apparently, multipole expansions are not useful
in this case since multipole expansions are useful to take into account
long range effects, but not short range effects.


# Vector: Arthur Whitney and text editors

- http://archive.vector.org.uk/art10501320


# Representing CPS in LLVM using the `@coro.*` intrinsics

This is part of a larger thread --- [Adding CPS call support to LLVM](http://lists.llvm.org/pipermail/llvm-dev/2017-April/112212.html) where there is a large discussion on the correct design of how to teach LLVM about CPS.

Gor Nishanov proided the above example of encoding CPS using the llvm `coro` instructions.

- https://gist.github.com/bollu/e0573dbc145028fb42f89e64c6dd6742

# Bug in the LLVM code generator: Lowering of `MO_Add2` and `MO_AddWordC`

[Both of these are lowered the same way](https://github.com/ghc/ghc/blob/bf73419518ca550e85188616f860961c7e2a336b/compiler/llvmGen/LlvmCodeGen/CodeGen.hs#L817),
but they should be different. 

In particular, `GHC.Prim` explains:
- [`AddWordC#`](http://hackage.haskell.org/package/ghc-prim-0.5.3/docs/GHC-Prim.html#v:addWordC-35-) returns `(result, carry)`
- [`PlusWordC#`](http://hackage.haskell.org/package/ghc-prim-0.5.3/docs/GHC-Prim.html#v:plusWord-35-) returns `(carry, result)`

Honestly, this is confusing, but I guess there's some story to having two separate primops for this?


# Discrete random distributions with conditioning in 20 lines of haskell:

```hs
newtype D a = D { unD :: [(a, Double)] } deriving(Eq, Show, Ord)

instance Functor D where
    -- fmap :: (a -> b) -> D a -> D b
    fmap f (D xs) = D $ fmap (\(a, d) -> (f a, d)) xs

instance Monad D where
    return x = D $ [(x, 1.0)]
    -- f :: a -> (D b)
    (D as) >>= f = D $ do -- list monad
                      (a, p) <- as
                      (b, p2) <- unD (f a)
                      return $ (b, p * p2)

-- [(a, 0.5), (b, 0.5)]
-- [(a, 0.3), (a, 0.2), (b, 0.1), (b, 0.4)]
--
instance Applicative D where
    pure = return
    ff <*> fa = do
        f <- ff
        a <- fa
        return $ f  a

condition :: Bool -> D ()
condition True = D [((), 1.0)]
condition False = D [((), 0.0)]


dice :: D Int
dice = let p = 1.0 / 6 in D $ [(x, p) | x <- [1..6]]


dice_hard :: D Int
dice_hard = do
    x <- dice
    condition $ x > 3
    return $ x


main :: IO ()
main = do
    print dice
    print dice_hard
```

This gives the output:

```
D {unD = [(1,0.16666666666666666),
          (2,0.16666666666666666),
          (3,0.16666666666666666),
          (4,0.16666666666666666),
          (5,0.16666666666666666),
          (6,0.16666666666666666)]}
          
D {unD = [(1,0.0),
          (2,0.0),
          (3,0.0),
          (4,0.16666666666666666),
          (5,0.16666666666666666),
          (6,0.16666666666666666)]}
```

Notice that `D a ~= WriterT (Product Float) []`!

# Everything you know about word2vec is wrong.

The classic explanation of `word2vec`, in skip-gram, with negative sampling,
in the paper and countless blog posts on the internet is as follows:

```
while(1) {
   1. vf = vector of focus word
   2. vc = vector of context word
   3. train such that (vc . vf = 1)
   4. for(0 <= i < negative samples):
           vneg = vector of word *not* in context
           train such that (vf . vneg = 0)
}
```

Indeed, if I google "word2vec skipgram", the results I get are:
- [The wikipedia page which describes the algorithm on a high level](https://en.wikipedia.org/wiki/Word2vec#Training_algorithm)
- [The tensorflow page with the same explanation](https://www.tensorflow.org/tutorials/representation/word2vec)
- [The towards data science blog which describes the same algorithm](https://towardsdatascience.com/word2vec-skip-gram-model-part-1-intuition-78614e4d6e0b)
the list goes on. However, __every single one of these implementations is wrong__.

The original word2vec `C` implementation does _not_ do what's explained above,
and is _drastically different_. Most serious users of word embeddings, who use
embeddings generated from `word2vec` do one of the following things:

1. They invoke the original C implementation directly.
2. They invoke the `gensim` implementation, which is _transliterated_ from the
   C source to the extent that the variables names are the same.

Indeed, the `gensim` implementation is the _only one that I know of which 
is faithful to the C implementation_.

### The C implementation

The C implementation in fact maintains _two vectors for each word_, one where
it appears as a focus word, and one where it appears as a context word. 
(Is this sounding familiar? Indeed, it appears that GloVe actually took this
idea from `word2vec`, which has never mentioned this fact!)

The setup is incredibly well done in the C code:

- An array called `syn0` holds the vector embedding of a word when it occurs
as a _focus word_. This is __random initialized__. 

```cpp
https://github.com/tmikolov/word2vec/blob/20c129af10659f7c50e86e3be406df663beff438/word2vec.c#L369
  for (a = 0; a < vocab_size; a++) for (b = 0; b < layer1_size; b++) {
    next_random = next_random * (unsigned long long)25214903917 + 11;
    syn0[a * layer1_size + b] = 
       (((next_random & 0xFFFF) / (real)65536) - 0.5) / layer1_size;
  }

```

- Another array called `syn1neg` holds the vector of a word when it occurs
as a _context word_. This is __zero initialized__.

```cpp
https://github.com/tmikolov/word2vec/blob/20c129af10659f7c50e86e3be406df663beff438/word2vec.c#L365
for (a = 0; a < vocab_size; a++) for (b = 0; b < layer1_size; b++)
  syn1neg[a * layer1_size + b] = 0;
```

- During training (skip-gram, negative sampling, though other cases are 
also similar), we first pick a focus word. This is held constant throughout
the positive and negative sample training. The gradients of the focus vector
are accumulated in a buffer, and are applied to the focus word 
_after it has been affected by both positive and negative samples_.

```cpp
if (negative > 0) for (d = 0; d < negative + 1; d++) {
  // if we are performing negative sampling, in the 1st iteration,
  // pick a word from the context and set the dot product target to 1
  if (d == 0) {
    target = word;
    label = 1;
  } else {
    // for all other iterations, pick a word randomly and set the dot
    //product target to 0
    next_random = next_random * (unsigned long long)25214903917 + 11;
    target = table[(next_random >> 16) % table_size];
    if (target == 0) target = next_random % (vocab_size - 1) + 1;
    if (target == word) continue;
    label = 0;
  }
  l2 = target * layer1_size;
  f = 0;

  // find dot product of original vector with negative sample vector
  // store in f
  for (c = 0; c < layer1_size; c++) f += syn0[c + l1] * syn1neg[c + l2];

  // set g = sigmoid(f) (roughly, the actual formula is slightly more complex)
  if (f > MAX_EXP) g = (label - 1) * alpha;
  else if (f < -MAX_EXP) g = (label - 0) * alpha;
  else g = (label - expTable[(int)((f + MAX_EXP) * (EXP_TABLE_SIZE / MAX_EXP / 2))]) * alpha;

  // 1. update the vector syn1neg,
  // 2. DO NOT UPDATE syn0
  // 3. STORE THE syn0 gradient in a temporary buffer neu1e
  for (c = 0; c < layer1_size; c++) neu1e[c] += g * syn1neg[c + l2];
  for (c = 0; c < layer1_size; c++) syn1neg[c + l2] += g * syn0[c + l1];
}
// Finally, after all samples, update syn1 from neu1e
https://github.com/tmikolov/word2vec/blob/20c129af10659f7c50e86e3be406df663beff438/word2vec.c#L541
// Learn weights input -> hidden
for (c = 0; c < layer1_size; c++) syn0[c + l1] += neu1e[c];
```

### Why random and zero initialization?

Once again, since none of this actually explained in the original papers
_or on the web_, I can only hypothesize.

My hypothesis is that since the negative samples come from all over the text
and are not really weighed by frequency, you can wind up picking _any word_,
and more often than not, _a word whose vector has not been trained much at all_.
If this vector actually had a value, then it could move the actually important
focus word randomly. 

The solution is to set all negative samples to zero, so that _only vectors
that have occurred somewhat frequently_ will affect the representation of
another vector.

It's quite ingenious, really, and until this, I'd never really thought of
how important initialization strategies really are.


### Why I'm writing this

I spent two months of my life trying to reproduce `word2vec`, following
the paper exactly, reading countless articles, and simply not succeeding.
I was unable to reach the same scores that `word2vec` did, and it was not
for lack of trying.

I could not have imagined that the paper would have literally fabricated an
algorithm that doesn't work, while the implementation does something completely
different.

Eventually, I decided to read the sources, and spent three whole days convinced
I was reading the code wrong since literally everything on the internet told me
otherwise.

I don't understand why the original paper and the internet contain zero
explanations of the _actual_ mechanism behind `word2vec`, so I decided to put
it up myself.

This also explains GloVe's radical choice of having a separate vector
for the negative context --- they were just doing what `word2vec` does, but
they told people about it `:)`.

Is this academic dishonesty? I don't know the answer, and that's a heavy
question. But I'm frankly incredibly pissed, and this is probably the last
time I take a machine learning paper's explanation of the algorithm
seriously again --- from next time, I read the source _first_. 

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

-- Probabilities
-- ============
type F = Float
-- | probability density
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

I wanted to use debug info to help build a better debugging experience
within [`tweag/asterius`](http://github.com/tweag/asterius). So, I was 
reading through the sources of `cmm/Debug.hs`.  
I'd never considered how to debug debug-info, and I found the information
tucked inside a cute note in GHC (`Note [Debugging DWARF unwinding info]`):

> This makes GDB produce a trace of its internal workings. Having gone this far,
> it's just a tiny step to run GDB in GDB. Make sure you install debugging
> symbols for gdb if you obtain it through a package manager.


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
manifold, and `cm`("chart for "m") is a continuous mapping from `O` to `R^n`
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

##### Vector field as derivation

TODO

##### Lie derivation

##### Lie derivation as lie bracket




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

```cpp
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

1. Each finite intersection is non-empty, and will be equal to the set that
   has the highest index in the finite intersection.

2. Each of the sets `Si` are compact since they are closed and bounded subsets of `R`

3. Invoke theorem.


## Japanese Financial Counting system
- [Wikipedia](https://en.wikipedia.org/wiki/Japanese_numerals#Formal_numbers)

Japanese contains a separate kanji set called `daiji`, to prevent people
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
implementation of Word2Vec, with negative sampling, without hierarchical
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
Core building block of effectively using the ellipsoid algorithm.

- If we posess a way to check if a point $p \in P$ where $P$ is a polytope, we
  can use this to solve optimisation problems.
- Given the optimisation problem maximise $c^Tx$ subject to $Ax = b$, we can
  construct a new _non-emptiness_ problem. This allows us to convert optimisation
  into _feasibility_.
- The new problem is $Ax = b, A^Ty = c, c^Tx = b^T y$. Note that by duality,
  a point in this new polyhedra will _be an optimal solution to the above linear program_.
  We are forcing $c^Tx = b^Ty$, which will be the optimal solution, since the
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
