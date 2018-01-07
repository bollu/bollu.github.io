+++
title = "Papers I Read and Loved in 2017"
date = "2018-01-05T18:20:53+05:30"
draft = true
+++

I spent 2017 as a research intern working at the systems group, ETH Zurich. I worked on Polly, a loop
optimizer for LLVM, so this had me read a bunch of compiler related papers.

I also started implementing [simplexhc](https://github.com/bollu/simplexhc-cpp), a STG to LLVM compiler. STG is the Glasgow Haskell compiler's internal
representation for Haskell. This had me reading a bunch of *lazy* compiler papers!


I decided to make a list of papers I read, what I loved about them, and which ones I totally did not get.

## [Futhark: Purely functional GPU programming](https://futhark-lang.org/publications/pldi17.pdf)
I read the paper, and I absolutely loved it. They describe a language, futhark, which is purely functional
and compiles to GPUs.

This was around the time I had started rewriting my compiler from Haskell to C++. Since futhark was written in C++,
I began reading their source code, which was quite fun.

Some cute things in their codebase:

- [The diet of a function is the kinds of arguments it accepts]()
- [The type parameter that stores context is called "lore"]()

## [STG: Implementing lazy functional programming languages on stock hardware](https://www.dcc.fc.up.pt/~pbv/aulas/linguagens/peytonjones92implementing.pdf)
A classic paper describing the design of the "abstract machine" that GHC compiles to. It's well written, but is quite dense.
I didn't quite understand much of it till I started implementing my own STG compiler. I would often miss cases, and come back to the
paper to see what I had missed.


[STGi is a really good tool to understand STG evaluation. ](https://github.com/quchen/stgi)

## [GRIN: a highly optimising backend for lazy functional languages](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.27.3918&rep=rep1&type=pdf)

It's an interesting take on an alternate intermediate representation for Haskell / lazy languages. I do not claim to fully understand the paper.

To me, the main message was that if one can represent the act of **forcing** a thunk to be evaluated (what GRIN calls `eval`), then one
can allow optimisations within the compiler to automatically perform strictness analysis.

It's a very interesting idea to be sure.

In discussions with Edward Kmett, he mentioned the possibility of starting from an STG like substrate, and then moving to GRIN within a tracing JIT.
This is a very cool idea, and I plan on pursuing this within simplexhc.

## [Optimistic Evaluation: a fast evaluation strategy for non-strict programs](https://www.microsoft.com/en-us/research/publication/optimistic-evaluation-fast-evaluation-strategy-non-strict-programs/)

It's a cool paper describing how to evaluate lazy values "ahead of time", and not run into nasty situations that one could get, by eg. forcing bottoms,
or forcing I/O.

They seem to get *significant* speedups in many cases, so the technique is something I definitely wish to adopt in simplexhc. If anyone knows
what happened to this within GHC, I'd love to know.

## [Defunctionalizing Push arrays](https://svenssonjoel.github.io/writing/defuncEmb.pdf)

Another paper I do not claim to understand.

The paper describes two forms of encoding arrays:
- Pull arrays, which are basically `data Pull a = Pull { length :: Nat, getData :: Ix -> a}`. That is, a combination of length info
  and a getter for the nth value.
- Push arrays, which are `data Push a = Push { length :: Nat, runConsumer :: -> (Ix -> a -> SomeMonad ()) -> SomeMonad ()}`, which is a continuation based representation.
It's a combination of length info and a function `runConsumer`, which takes a continuation `cont :: Ix -> a -> SomeMonad()`. `cont` receives the
index and value of all elements in the array and gets to do whatever side-effects it wants `SomeMonad`.

They perform defunctionalization on the push array representation. Defunctionalization is a technique that takes a higher order function, and converts it to a function
that operates on concrete data. Hence, we eliminate all higher order functions in the language.


## [Defunctionalization at work](http://www.daimi.au.dk/~danvy/DSc/22_danvy-nielsen_ppdp-2001.pdf)


Defunctionalization is a technique that takes a higher order function, and converts it to a function
that operates on concrete data. Hence, we eliminate all higher order functions in the language. This paper describes this technique in a couple of contexts.

I didn't read this too deeply, but I'm glad I have the reference around in case I wish to read more about it.


## [Live range reordering](http://impact.gforge.inria.fr/impact2016/papers/impact2016-verdoolaege.pdf)
This is a polyhedral technique that's used to reorder or privatize variables to allow for more parallelism.

Consider the code:

```C
int x = 0;
int A[1000];

for(int i = 0; i < 1000; i++) { 
    x += 1;
    A[i] = x;
    x = 100;
}
```

One might think that there is a scalar dependence across loop iterations (due to `x`), but live range
reordering exposes the fact that this inter-loop dependence actually does not exist. The code can
be transformed to:


```C
int A[1000];

for(int i = 0; i < 1000; i++) {
    int x = 0;
    if (i != 0) { x = 101;}
    A[i] = x;
}
```

The paper describes the implementation of the technique within the polyhedral model and benchmarks.


## [Polyhedral AST generation is more than just scanning polyhedra](https://lirias.kuleuven.be/bitstream/123456789/497238/1/toplas-astgen.pdf)

code generation in the polyhedral model was usually seen as looping / scanning over the polyhedra in some correct order.

In this paper, they instead show to convert from a polyhedra to an AST based representation. This representation is both easier to work
with, in terms of API, and also allows interesting optimizations which would be harder in the "vanilla" representation.

The only reason I read this was because I was working on [Polly](http://polly.llvm.org/), which uses the "schedule trees" (AST representation of polyhedra).

It's certainly a niche paper.

## Deforestation: Transforming programs to eliminate trees
- [Wikipedia link with paper (pdf)](https://en.wikipedia.org/wiki/Deforestation_(computer_science))

Deforestation is a neat way to analyse a program to eliminate intermediate data structures. It's a simple syntactic rule, and the 
normalization process is also quite simple - it's a recursive rule.

The extend the algorithm, to one called "blazed deforestation", where they restrict the rules for which terms can be deforested.

Once again, I know of no GHC numbers for this technique. If someone knows, please do leave a comment!



## Copatterns: Programming infinite structures by observations

Paper I did not understand beyond the original idea.

The idea seems to be to look at co-inductive data structures as "experiments", and we are allowed to view
"observations" from these experiments.

The type theory that occurs after the paper is beyond me. I was disappointed, because I wanted to understand the idea.
If there's a more accessible discussion surrounding this, I would like to know.

## [Making a fast curry: Push/Enter versus Eval/Apply](http://www.cs.tufts.edu/~nr/cs257/archive/simon-peyton-jones/eval-apply-jfp.pdf)

This paper handles compiling function applications in lazy languages. There are two major flavours of techniques:

- Eval/Apply, The **caller** inspects the callee and pushes the right number of arguments on the stack, creating continuations if there are more parameters than required.

- Push/Enter, The **caller** simply pushes all arguments onto the stack. The **callee** knows its arity, and thus it snoops the stack to pick up the correct number of arguments.

it's quite possible I messed up the explanation, because I didn't read the paper in detail. The difference seems quite small, so I implemented whatever made sense to me `:)`. As the paper says, "There  is  not  much
to choose between the two models on performance grounds".

## [The Intel Labs haskell research compiler](https://github.com/IntelLabs/flrc).

This is a description of Intel's haskell compiler. They use an interesting substrate for the language which is neither GRIN nor STG.

The use an SSA-based internal representation within the compiler. 
The primary idea is to harness immutability and memory safety to implement wildly aggressive optimizations.

They perform run-of-the-mill dataflow based optimizations like dead code elimination and constant propagation.
They also implement a points-to analysis to optimise object representations.

They claim that optimisations such as vectorization, and in general, memory analysis is vastly simplified due to immutability. I haven't read or benched their compiler, so I don't know, but it sounds plausible. 

It's a really interesting point in the design space, for sure.

## [Garbage collection - Immix, a mark region GC](http://www.cs.utexas.edu/users/speedway/DaCapo/papers/immix-pldi-2008.pdf)

As I was implementing simplexhc, I found the need to understand how to read garbage collection. Immix is a popular GC algorithm, and
[scala-native](http://www.scala-native.org/en/latest/) implements a GC that works with [LLVM](https://github.com/scala-native/immix) (which is the representation I compile to).

So, I wound up reading the Immix GC Algorithm. I don't know much about garbage collection. What I got out of the paper is that GCing is trade-off based.

- Mark/sweep marks memory regions that need to be freed, and then cleans it up. This  leads to fragmentation.
- Mark/compact marks memory regions and then "compacts" this non-free memory. This removes fragmentation, but winds up
requiring a destination location to copy the memory into.

Immix mixes the passes of marking dead stuff and copying into the compact region into a single pass. They provide a detailed analysis of the GC which I didn't have the background to fully appreciate.

## Tried reading [bananas, lenses and barbed wire](https://maartenfokkinga.github.io/utwente/mmf91m.pdf), gave up again.
This paper introduces recursion schemes, which a way to separate the pattern of recursion over the data from the actual
recursive computation that is being performed. This generalizes and unifies things such as `fold`, `unfold`, etc.

This is a paper I try reading every year, but give up because of the huge amounts of new notation. Last year was no different.

I wish the function signatures and things like that did not use the whole banana-brackets notation. Writing stuff in terms of `Fix` also takes a while to get used to.

In general, this is a paper I would like to grok, but keep failing at.

## [Hoopl: A modular, reusable library for dataflow analysis and optimization](https://www.cs.tufts.edu/~nr/pubs/hoopl-abstract.html)
I was reading the GHC sources, and kept running across Hoopl, which seemed to be a high-level way to spec out compiler transforms. It looked
interesting, so I wound up reading the paper.

It's a well written paper that motivates the Hoopl framework. The big takeaway from this was the fact that given nice algebras over the optimizations one is trying to perform (say, a join lattice that represents data and a monotone function that "adds" optimizations / information), one can come up with an elegant, general-purpose framework to find fixpoints of these "optimization functions" across different levels of the compiler - over instructions, basic blocks, and functions.

They introduce the nice notion of a `Shape` which shows how control will flow into and out of the unit of code. Roughly, to be `Open` means to have control flow fall-down (think C `switch-case` style) to the next item.

- The first instruction in a basic block will be `Closed` on entry and `Open` on exit.
- Something in the middle will be `Open` on both entry and exit.
- The final instruction will be `Open` on entry and `Closed` on exit.
 
## [LVars: Lattice based data structures for deterministic parallelism](https://www.cs.indiana.edu/~lkuper/papers/lvars-fhpc13.pdf)
This is very reminiscent of the Hoopl paper, where once again we take a join lattice over data structures that knows how to "combine" 
information, and show how this algebra can be used to create a library that exposes task-based parallelism. Moreover, this is deterministic,
so one can have much more confidence in the correctness of our algorithm, is what I understood.
 
I have not dabbled with the library, but it is on my to-do-list.
 
## [Reagents: Expressing and composing fine-grained concurrency](https://people.mpi-sws.org/~turon/reagents.pdf)

The LVars repository  mentions "implement reagents", which led me to this paper. It's a Scala based paper where they introduce "reagents", which
is a tiny composition-based library for writing concurrent algorithms. They use colorful chemistry names in their library (eg: `reagent`, `catalyst`, etc.)

It was a fun read, but I didn't really get this either, because I've never been in a situation where I would felt the
need for such a library.

## [Abstract interpretation: a unified lattice model for static analysis of programs by construction or approximation of fixpoints](http://www.di.ens.fr/~cousot/COUSOTpapers/POPL77.shtml)

Abstract interpretation is a really nice mathematical framework to unite ideas of static analysis.

The core idea is to consider two functions, one that "abstracts" away data from the program, and one that "concretizes" abstract
information to sets of possible values.

An example would be going from `Z` to `{even, odd}`

the abstraction function would be:

```py
abstract(n) = even if n % 2 == 0 else odd
```

the generalization function would be:
```py
concretize(even) = {..., -2, 0, 2, ...}
concretize(odd) = {..., -1, 1, 3, ...}
```

We ask for certain axioms on these two functions:

- `abstract(concretize(x)) = x` for all x in the abstract domain concretization does not remove information
- `x \in concretize(abstract(x))` abstraction generalizes the concrete domain.

Equipped with these functions, we can build a general theory as to how they work, certain ideas related to implementing this
concretely, etc.

It's a paper I really liked, and I loved how they show many things that compiler do, such as data flow analysis and interval analysis
of number-type variables are just.. abstract interpretation. It was also reminiscent of Hoopl.

A compiler based purely on Hoopl and abstract interpretation would be interesting, and quite possibly the only elegant *optimizing* compiler in existence `:)`

## [Tarski's original fixpoint theorem paper (A lattice theoretical fixpoint theorem and its applications)](https://msp.org/pjm/1955/5-2/pjm-v5-n2-p11-s.pdf)
Abstract interpretation refers to this paper, so I decided to read it. I didn't read all of it, just the theorems I care about.

The main result is that, given a monotone function over a lattice, once can show that this function will always have least and greatest fixed points.

Tarski shows how to use this in different contexts, but I only cared about how it is used in Abstract Interpretation.


## [The Octagon abstract domain](https://www-apr.lip6.fr/~mine/publi/article-mine-HOSC06.pdf)

This is an example of a "domain" of abstract interpretation. In the previous example, I had used `{even, odd}` as the abstract domain. The octagon domain is a certain kind of abstract domain that can represent octagonal shapes in `R^n`, and knows how to join octagons together.

It's a popular domain for abstract interpretation because it provides a nice balance between performance and power.

The paper referred backed to work that the same group did, so I did not really bother reading through the math. I plan on doing this if I ever have serious need of abstract interpretation.


## How to twist pointers without breaking them

This is a cool paper that is in context of [Raaz](), the haskell cryptographic library.

They describe monoid homomorphisms and how one can look at pointer operations as monoid homomorphisms. This was
a neat way of looking at things, and I'm more convinced that monoids are an interesting algebraic structure
after this. (The first thing that seriously made the case for me was paper for the `diagrams` library).



## [Derivative  regular type is Type of one-hole context](http://strictlypositive.org/diff.pdf)
## [Differentiatiyeng data structures](http://www.cs.nott.ac.uk/%7Epsztxa/publ/jpartial.pdf)

Both these papers show a really nice technique: If one rewrites an inductive type as some "formal" polynomial
and then differentiates this polynomial, we get a new polynomial which can be interpreted as the original type,
with a "hole" in it.

Let's take some example for this:

```hs
List = Nil | Cons Int List
```

To go to the polynomial space, I will set `List = l`, `Int = x`. Type sums becomes `+`,
Type products become `*`. A constructor like `Nil` becomes `1`, because it's presence is just
`1 bit` of information: it is either present, or it is not.

As a polynomial, this would be:

```

l = 1 + x * l
l(1 - x) = 1
l = 1/(1 - x)
```

This is already interesting, because we can reinterpret `1/(1 - x)` as follows:

```
l = 1/(1 - x)
l = 1 + x + x^2 + x^3 + ...
```

Which precisely matches our notion of a list! That is, it either has
`1` (which is `Nil`), OR just an Int value (which is `x`), OR two int values (which is `x^2`), OR ...


Now, let us differentiate this with respect to "x", because we wish to create a hole of "Int" in this.

```
l = 1/(1 - x)
dl/dx = 1/(1 - x)^2 = 1/(1 - x) * 1/(1 - x)
```

This can be seen as so:
```hs
data ListWithHole = ListWithHole (List Int, List Int)
```

This makes sense, because a "list with a hole" would just be the left and right hand parts of the list!

So, for example, a list could be `[1, 2, 3]`. A hole at `2` would give us `ListWithHole [1] [3]`.


This process will work on any data type, and it is very cool that this _does_ work! The paper goes into more detail into the process of constructing this.


#### Motivation for me reading it (rambing about polyhedral analysis and haskell):
I was looking into ways to try and fit polyhedral analysis tools into a Haskell-like context. I kept
running into the issue of representing a "location" in a data structure.

For some motivation, consider a loop in C:

```c
for(int i = 0; i < 1000; i++) {
  Stmt0:   A[i * 2 + 3] = 1000;
}
```

In Polly (and in polyhedral analysis in general), we would represent this loop as follows:

```
Stmt0
Domain: { i :  0 <= i <= 1000 }
Writes: { i -> A[i * 2 + 3] }
```

That is, the domain is an affine set.
Writes (which represents memory writes in the loop) is an affine map from each time point (`i`) to the
array index `A[i * 2 + 3]`.

A natural question is to ask: is it possible to derive similar information on operations on haskell?
For example, consider a map:

```hs
l :: [Int]
l = [1..1000]

l' :: [Int]
l' = map (+1) l
```

In theory, one can describe the `l'` computation as:

```
Writes: {l[i] -> l'[i]}
```

As an affine map that takes `l[i]` at each time point to `l'[i]`.

So, in theory, I need to be able to find "contexts" and then use these to represent haskell functions in terms
of these "contexts".

## [Projections for Strictness Analysis](https://link.springer.com/chapter/10.1007/3-540-18317-5_21)

This falls into the "compiler" reading.

A major problem in compiling lazy languages it that laziness is slow on contemporary hardware. So, optimising compilers for lazy languages perform *strictness analysis* - analyse which expressions can be computed strictly, and then do so! 

Clearly, this is not as easy as it sounds. 

The general idea of the paper is to identify _projection functions_, that on being applied to the original function do not change its semantics. Thus, one can describe parameter use based on the _projection_ that can be used with it.

The paper is really well written. It's funny, full of quite quips, and the math is well motivated. I loved reading this one. I don't grok the idea fully, but I'm comfortable with the basic idea written in the paper.

## [Fast Algorithms for Dyck-CFL-Reachability with Applications to Alias Analysis](https://helloqirun.github.io/papers/pldi2013_qirun.pdf)

I was looking into alias analysis for some polly work. I wound up reading this because a coworker ([Phillip Pfaffe]()) told me that this is one of the algorithms for alias analysis in LLVM.

The idea is really neat. One can look at alias analysis as a certain kind of reachability problem on a graph, and this problem and a fast solution is described in the paper. I didn't read this too deeply either, because I didn't need the solution, but it's nice to know this paper exists.

## [ScalarEvolution / Chains of Recurrences - a method to expedite the evaluation of closed-form functions](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.43.8188&rep=rep1&type=pdf)

LLVM has a nifty analysis called "ScalarEvolution(SCEV)", which "evolves" loop variables across loop iterations. It creates algebraic expressions that shows what a variable's value is at some loop iteration.


The basic idea is to come up with a bottom-up algorithm that first converts loop induction variables into a custom data structure, and then show how to combine smaller versions of these into larger ones.

It's a really clean idea, and it works surprisingly well, even in the face of complexity. The LLVM scalar evolution module is large, but is based on this core idea, with a bunch of extensions to handle "real world" cases and patterns.

## [Register allocation for Programs in SSA form](http://www.rw.cdl.uni-saarland.de/~grund/papers/cc06-ra_ssa.pdf)

In general, one can look at register allocation as graph coloring. In fact, these problems are isomorphic, so register allocation is NP-hard.

However, it was recently proved that *register allocation for SSA* can be performed in polynomial time.
I found this result very intriguing, so I read the paper.

The reason we can polynomial from what I can tell is because PHI nodes in SSA wind up "simplifying" the coloring problem. Hence, the graph has a nice property (of being chordal). On chordal graphs, greedy coloring is optimal. Thus, we can simply perform greedy coloring over SSA directly. 

However, this is not great in terms of performance, because it could lead to register shuffling on every loop iteration for example (the PHI node could be colored different from the incoming values into the PHI node).

## [Linear scan register allocation](http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf)

The original paper that introduced the current standard register allocation algorithm which uses live ranges to perform allocation. It's a short nice read.

However, there was some recent discussion on LLVM-dev about register allocation called [Register Allocation Graph Coloring algorithm and Others](https://groups.google.com/forum/#!topic/llvm-dev/R3VOsHAg9qw). The discussion revolved around how choosing what and how to spill values (values that are written back to memory when we run out of registers) is more important that optimal register allocation itself! There was sentiment that this is a problem that is not really addressed in the literature very well.


## [Extensible effects](http://okmij.org/ftp/Haskell/extensible/exteff.pdf)

I finally got around to reading the alternative to MTL (monad transformer library). The idea is to not look at monads as some kind of "effectful computation", but to look at it as a "client-server" model, where the client makes requests to the "effect server" which knows how to perform what is being requested for.

for example:

- the `Reader` client asks the server for the reader value on an `ask`.
- `Writer` asks the server to write the monoidal value on `put`.


From what I have read, it seems to solve the `n^2` instances problem of MTL, while allowing for a cleaner conceptual framework for effects, at least in my head. 

## [Equality saturation - a new approach to optimzations](http://www.cs.cornell.edu/~ross/publications/eqsat/)

It's a *really* cool idea. The idea is to not look at compiler optimisation as "phases", since that causes us to suffer from phase ordering problems.

Rather, we only ever add information to a CFG-like structure. However, this could quite clearly cause blowup. So, we *quotient* the tree with equivalence classes of things we know can be equivalent (from, say, algebraic rules). This (supposedly) controls the exponential growth that is possible in the worst case.

I don't fully understand the control growth bit, but the idea is nice and it's well presented. I have a half-done branch somewhere where I started implementing this in LLVM. I need to get back to this at some point in time.


## Rounding up

I read a bunch of compilers, and I loved it. This year, I wish to spend on theorem proving and lazy compiler construction. I hope to keep writing about the little things I find as I keep going along :)
