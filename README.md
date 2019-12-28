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

#### Table of contents:

- [An invitation to homology and cohomology](#an-invitation-to-homology-and-cohomology)
- [Stuff I learnt in 2019](#stuff-i-learnt-in-2019)
- [A motivation for p-adic analysis](#a-motivation-for-p-adic-analysis)
- [Line of investigation to build physical intuition for semidirect products](#line-of-investigation-to-build-physical-intuition-for-semidirect-products)
- [Topology is really about computation --- part 2](#topology-is-really-about-computation--part-2)
- [Topology is really about computation --- part 1](#topology-is-really-about-computation--part-1)
- [PSLQ algorithm: finding integer relations between reals](#pslq-algorithm-finding-integer-relations-between-reals)
- [Geometric characterization of normal subgroups](#geometric-characterization-of-normal-subgroups)
- [Radical ideals, nilpotents, and reduced rings](#radical-ideals-nilpotents-and-reduced-rings)
- [My disenchantment with abstract interpretation](#my-disenchantment-with-abstract-interpretation)
- [Computing equivalent gate sets using grobner bases](#computing-equivalent-gate-sets-using-grobner-bases)
- [The janus programming language --- Time reversible computation](#the-janus-programming-language--time-reversible-computation)
- [`A = B` --- A book about proofs of combinatorial closed forms (TODO link)](#TODO)
- [Generating `k` bitsets of a given length `n`](#generating-k-bitsets-of-a-given-length-n):
- [Bondi k-calculus](#bondi-k-calculus) (Check link!)
- [Vivado toolchain craziness ](#vivado-toolchain-craziness)
- [What the hell _is_ a Grobner basis? Ideals as rewrite systems](#what-the-hell-is-a-grobner-basis-ideals-as-rewrite-systems)
- [Lie bracket versus torsion](lie-bracket-versus-torsion)
- [Spatial partitioning data structures in molecular dynamics](#spatial-partitioning-data-structures-in-molecular-dynamics)
- Vector: Arthur Whitney and text editors (TODO link)
- [Everything you know about word2vec is wrong (check link)](#everything-you-know-about-word2vec-is-wrong)
- [Small Haskell MCMC implementation (check link)](#small-haskell-MCMC-implementation)
- [Debugging debug info in GHC (check link)](#debugging-debug-info-in-GHC)
- GHC LLVM code generator: Switch to unreachable (TODO link)
- Concurrency in Haskell (TODO link)
- [Handy list of differential geometry definitions](#handy-list-of-differential-geometry-definitions)
- Lazy programs have space leaks, Strict programs have time leaks (TODO link)
- [Presburger arithmetic can represent the Collatz Conjecture (check link)](#presburger-arithmetic-can-represent-the-collatz-conjecture)



# [An invitation to homology and cohomology](#an-invitation-to-homology-and-cohomology)

There are many introductions to homology on the internet, but none of them
really met my criteria for being simple, picture filled, and getting the
basic ideas across. I feel that Hatcher might come closest to what I want 
(and where I originally learnt the material), but their description of homology
is surrounded by the context of Algebraic Topology, while really, simplicial
homology is accessible to anyone who has seen some linear algebra and group
theory. This is my attempt to get the ideas across.

Let's first try to understand what we're trying to do here. We want to detect
holes in a space, broadly construed. We focus in _simplicial complexes_, which
are collections of triangles and triangle-like objects in higher (and lower)
dimensions. We define what holes are for these triangles, and we try to
find algebraic objects that allow us to "detect" these holes.

<img src="https://raw.github.com/bollu/bollu.github.io/master/static/simplices/simplices.svg?sanitize=true">

### Simplices
- A 0-simplex is a point
- A 1-simplex is a line
- A 2-simplex is a tetrahedron
- A $k$-dimensional simplex is the convex hull of $k+1$
  linearly independent points $(u_i \in \mathbb R^{k+1})$
  in $k+1$ dimensional space.
  $S_k \equiv \left\\{ \sum theta_i u_i \vert \theta_i \geq 0, ~ \sum_i \theta_i = 1 \\}$

### Simplicial complexes

A simplicial complex $K$ is a collection of simplices where:
- every face of a simplex from $K$ is in $K$
- The intersection of any two simplices in $K$ is also in $K$

Non-examples of simplicial complexes are:
<img src="https://raw.github.com/bollu/bollu.github.io/master/static/simplices/non-simplex-1.svg?sanitize=true">
- This does not contain the point at the lower-left corner, which should exist
  since it is a face of the straight line.

<img src="https://raw.github.com/bollu/bollu.github.io/master/static/simplices/non-simplex-2.svg?sanitize=true">
- This does not contain the points which are at the intersection of the
  triangle and the line.

### Holes in a space: Homology of a triangle

Let's consider the simplest possible case of computing the homlogy, and we
do so, we will expand on what homology _is_, and what we're trying to do.

<img src="https://raw.github.com/bollu/bollu.github.io/master/static/simplices/homology-triangle-edges.svg?sanitize=true">

Look at the triangle above. We have the red, green, and blue vertices, which
I'll notate $r, g, b$. We also have the edges that are orange ($o$), cyan ($c$), and
magenta ($m$).

What we are interested in doing is to be able to detect the "hole" in the
triangle in between the edges `o-m-c`. That is, we want some algorithm which
when offered the representation of the triangle, can somehow detect the hole.
Note that the hole doesn't really depend on the length of the edges. We can
"bend and stretch" the triangle, and the hole will still exist. The only way
to destroy the hole is to either _cut_ the triangle, or _fill in_ the triangle.


To detect the hole, we first describe the shape of the triangle in terms
of two groups, $E$ and $V$ representing the edges and the vertices, and
a linear operator $\partial_{EV}: E \rightarrow V$, called as the 
boundary operator, which tells us how edges
are glued to vertices.

We now define a group, $E \equiv \mathbb Z \times \mathbb Z \times \mathbb Z$
that represents linear combinations of edges. For example, $(1, 2, 3) \in E$
represents $o + 2m + 3c$ --- that is, take 1 copy of the orange edge, 2
copies of the magenta edge, and 3 copies of the cyan edge.

We define another group, $V \equiv \mathbb Z \times \mathbb Z \times \mathbb Z$
which represents linear combinations of vertices. For example,
$(1, -1, 2) \in V$ represents $r - g + 2b$ --- that is, take a copy of the
red vertex, subtract the green vertex, and add two copies of the blue vertex.


The boundary operator $\partial_{EV}: E \righarrow V$ is depicted in the
picture. This operator sends edges to their _boundary_, and is therefore called
the _boundary operator_.  The _boundary_ of an edge describes the edge in terms
of vertices, just like we would describe a direction vector (to borrow physics
parlance) by subtracting points.

The action of the operator on a linear combination of edges is:
$$
&\partial_{EV}: E \rightarrow V \\
&\partial_{EV}(1, 0, 0) \equiv (1, -1, 0) \qquad o \rightarrow r - g \\
&\partial_{EV}(0, 1, 0) \equiv (-1, 0, 1) \qquad m \rightarrow b - r \\
&\partial_{EV}(0, 0, 1) \equiv (0, 1, -1) \qquad c \rightarrow b - g \\
&\text{(Extend using linearity)} \\
&\partial_{EV}(s, t, u) \equiv 
  s \partial_{EV}(1, 0, 0) + 
  t \partial_{EV}(0, 1, 0) + 
  u \partial_{EV}(0, 0, 1) = (s - t, u - s, t - u)
$$

Now, notice that to traverse the cycle, we should traverse the orange edge, 
then the magenta edge, then the cyan edge, in that direction. That is,
the cycle can be thought of as $o + m + c$. However, how do we _detect_ this
cycle? The key idea is that if we look at the 
_image of the cycle under the boundary operator_ $\partial_{EV}$,
we will get $0$! For us to have completed a cycle, we must have both
entered and exited each vertex, so the total sum must be 0.

This is very nice, since we have converted the topological invariant
of a _hole in the space_ into an algebraic invariant of "linear combination
of edges that map to 0". That is, we want to consider all thoose loops
that belong to the _kernel_ of $\partial_{EV}$. (Terminology: 
the kernel of a linear transformation / group homomorphism
is the set of all things which map to zero.)

So, we define (tentatively) the first homology group:
- $H_1 \equiv Kernel(\partial_{EV}) \subset E$

If we try to compute this, we will have to have:
$$
H_1 &\equiv Kernel(\partial) \\
&= \\{ (s, t, u) | \partial(s, t, u) = (0, 0, 0) ~ s, t, u \in \mathbb Z \\} \\
&= \\{ (s, t, u) | (s-t, u-s, t-u) = (0, 0, 0) ~ s, t, u \in \mathbb Z  \\} \\
&= \\{ (s, t, u) | s = t = u ~ s, t, u \in \mathbb Z \\} \\
&= \\{ (x, x, x) | x \in \mathbb Z \\} \simeq \mathbb Z
$$

So, we know that we have a $\mathbb Z$ worth of cycles in our triangle, which
makes sense: We can go clockwise (positive numbers)
and counter-clockwise (negative numbers) around the triangle,
and we can go as many times as we wish, so we have $\mathbb Z$ as the
number of cycles.

that is, it's the linear combination of edges that map to zero through the
boundary map. Note that this also includes combinations such as _two_ loops
around the triangle, such as $o + m + c + o + m + c$.

### (No) Holes in a space: Homology of a _filled_ triangle

<img src="https://raw.github.com/bollu/bollu.github.io/master/static/simplices/homology-triangle-faces.svg?sanitize=true">

In this case, notice that the triangle is _filled_ with a face $f$.
Therefore, the "hole" that we had previously is now filled up, and does not
count anymore. So, we now need to amend our previous definition of $H_1$ to
kill the hole we had detected. 

The idea is that the hole we had previously is now the
_boundary of the new face $f$_.
Since it is the boundary of a "filled in" region, it does not count anymore,
since we can "shrink the hole" across the face to make it a non-loop.
Hence, we need to quotient our $H_1$ with the boundary of the face.

Formally, what we do is we create another group $F \equiv \mathbb Z$, which
counts copies of our face $f$, and we define another boundary operator, such
that the boundary of the face $f$ is $o + m + c$.

$$
&\partial_{FE} : F \rightarrow E \\
&\partial_{FE}(1) \equiv (1, 1, 1) 
&\text{(Extend using linearity)} \\
&\partial_{FE}(c) \equiv c \partial(1)
$$

Now, we should notice that the _image_ of $\partial_{FE}$ is a loop
$(o + m + c)$, which lies ie the _kernel_ of $\partial_{EV}$. This is a
general feature of homology, so it bears repeating:

- $image(\partial_{FE}) \subset kernel(\partial_{EV})
- $\partial_{FE} \circ \partial_{EV} = 0$
- The above equation is sometimes stylishly (somewhat misleadingly) written as
  $d^2 = 0$ or $\partial^2 = 0$.

Now, since the image of $\partial_{FE}$ lies entirely in the kernel of $\partial_{EV}$,
we can construct $H_1$ as:

$$
- $H_1 \equiv Kernel(\partial_{EV}) / Image(\partial_{FE}) \subset E$
$$

## Cohomology: Extending functions on the triangle

![cohomology-triangle-vertices](static/cohomology-triangle-vertices.svg)

Once again, we have our humble triangle with vertices $V = \\{r, g, b\\}$,
edges $E = \\{o, m, c \\}$, faces $F = \\{ f \\}$ with a boundary maps $\partial{EV}$,
$\partial_{FE}$:

- $\partial_{EV}(o) = r - g$
- $\partial_{EV}(m) = b - r$
- $\partial_{EV}(c)= g - b$
- $\partial_{FE}(f)= o + m + c$

We define a function $g_v: V \rightarrow \mathbb R$ on the vertices as:
-  $g_v(r) = 3$, $g_v(g) = 4$, $g_v(b) = 10$.

We now learn how to _extend_ this function to the higher dimensional objects,
the faces and the edges. To extend this function to the edges, we define
a new function:

- $g_e: E \rightarrow R$
- $g_e(e) \equiv \sum_i \alpha_i f(v_i)$ where $\partial_{EV} e = \sum_i \alpha_i v_i$

Expanded out on the example, we evaluate $df$ as:

- $g_e(o) \equiv \int g_v o  = f(r) - f(g) = 3 - 4 = -1$
- $g_e(m) \equiv \int g_v m  = f(b) - f(r) = 10 - 3 = +7$
- $g_e(c) \equiv \int g_v c  = f(g) - f(b) = 4 - 10 = -6$

More conceptually, we have created an _operator_ called $d$ which takes functions
defined on vertices to functions defined on edges:

- $d: (V \rightarrow \mathbb R) \rightarrow (E \rightarrow \mathbb R)
- $d(f_v) \equiv g_e$, $g_e(e) \equiv \sum_i \alpha_i f(v_i)$ where $\partial_{EV} e = \sum_i \alpha_i v_i$


We can repeat the construction we performed above, to construct another operator
$d : (E \rightarrow \mathbb R) \rightarrow (F \rightarrow \mathbb R)$, defined
in _exactly the same way_ as we did before.  For example, we can evaluate:

- $g_f \equiv d(g_e)$
- $g_f(f) \equiv  \int g_e f = g_e(o) + g_e(m) + g_e(c) = -1 + 7 -6 = 0$

What we have is a chain:

- $g_v \xrightarrow{d} g_e \xrightarrow{d} g_f$

Where we notice that $d^2 = 0$, since the function $g_f$ that we have gotten
evaluates to zero on the face $f$. We can prove this in general 
(it's a good exercise in definition chasing).


# Cohomology of an unfilled triangle

Here, we explore some terminology (closed and exact differential forms),
and we try to understand why this terminology is profitable.

![cohomology-triangle-face](static/cohomology-triangle-edges.svg)

#### Closed differential forms

# Cohomology of half-filled butterfly

![cohomology-half-filled-butterfly](static/cohomology-half-filled-butterfly)
Here, we have vertices $V \equiv \\{ r, g, b, b, p \\}$, edges
$E \equiv \\{rb, gr, bg, m, o, c \\}$ and faces $F \equiv \\{ f \\}$.

Here, we see a differential form $g_e$ that is defined on the edges,
and also obeys the equation $dg_e = 0$ (Hence is ???). However, it 
_does not have an associated potential energy_ to derive it from. That is,
there cannot exist a certain $g_v$ such that $d g_v = g_e$.

Hence, this $g$ that we have found is a non-trivial element of $ker d_{FE} / Im d_{EV}$,
since $dg_e = 0$, hence $g_e \in ker d_{FE}$, while there does not exist
a $g_v$ such that $d g_v = g_e$, hence it is _not quotiented_ by the image of
$d_{EV}$.

So the failure of the space to be fully filled in (ie, the space has a hole),
is measured by the _existence of a function $g_e$ that is closed but not exact!_

This reveals a deep connection between homology and cohomology, which is
made explicit by the [Universal Coefficient Theorem](TODO)

# Ideas I stumble onto

# [Stuff I learnt in 2019](#stuff-i-learnt-in-2019)

I write these retrospective blog posts every year since 2017. I tend to post a
collection of papers, books, and ideas I've stumbled across that year.
Unfortunately, this year, the paper list will be sparser, since I lost some
data along the way to the year, and hence I don't have links to everything I
read. So this is going to be a sparser list, consisting of things that I found
_memorable_.

I also re-organised my website, letting the link die, since keeping it up was
taking far too many cycles (In particular, CertBot was far too annoying to
maintain, and the feedback of hugo was also very annoying). I now have a 
_single_ file, the
[`README.md`of the `bollu/bollu.github.io`](https://github.com/bollu/bollu.github.io)
repo,
to which I add notes on things I find interesting. I've bound the `i` alias 
(for idea) on all my shells everywhere, to open the `README.md` file, wait
for me to write to it, run a `git commit` so I can type out a commit, and 
then push. This has been _massive_ for what I manage to write down: I feel
like I've managed to write down a lot of one-liners / small facts that I've
picked up which I would not otherwise. I'm attempting to similarly pare down
other friction-inducing parts of my workflow. Suggestions here would be very 
welcome!


If there's a theme of the year (insofar as my scattered reading has a
theme...), it's "lattices and geometry". Geometry in terms of differential
geometry, topology, and topoi. Lattices in the sense of a bunch of abstract
interpretation and semantics.

#### Course work: optimisation theory, quantum computation, statistics

My course work was less interesting to me this time, due to the fact that I had
chosen to study some wild stuff earlier on, and now have to take reasonable stuff
to graduate. However, there were courses that filled in a lot of gaps in my
self-taught knowledge for me, and the ones I listed were the top ones in that
regard.

I wound up reading
[Boyd on optimisation theory](https://web.stanford.edu/~boyd/cvxbook/), 
[Nielsen and Chuang](http://mmrc.amss.cas.cn/tlb/201702/W020170224608149940643.pdf) for quantum computation,
where I also
[solved a bunch of exercises in Q#](https://github.com/bollu/quantum-course-exercises) 
which was very fun and rewarding. I'm beginning to feel that learning quantum
computation is the right route to grokking things like entanglement and
superposition, unlike the physics which is first of all much harder due to
infinite dimensionality, and less accessible since we can't _program_ it.

#### Formal research work: Compilers, Formal verification, Programming languages

My research work is on the above topics, so I try to stay abreast of what's
going on in the field. What I've read over the past year on these topics is:


- [`A^2I`: meta-abstract interpretation](https://popl19.sigplan.org/details/POPL-2019-Research-Papers/71/A-2-I-Abstract-2-Interpretation).
  This paper extends the theory of abstract interpretation to perform abstract
  interpretation on program analyses themselves. I'm not sure how _useful_ this
  is going to be, as I still hold on to the belief that AI as a framework is
  too general to allow one to prove complex results. But I am still interested
  in trying to adapt this to some problems I have at hand. Perhaps it's going
  to work.


- [Cubicial Agda](https://dl.acm.org/citation.cfm?id=3341691). This paper introduces
  cubical type theory and its implementation in Agda. It appears to solve many
  problems that I had struggled with during my formalization of loop
  optimisations: In particular, dealing with Coinductive types in Coq, and that
  of defining quotient types / setoids. Supposedly, cubical Agda makes dealing
  with Coinduction far easier. It allows allows the creation of "real" quotient
  types that respect equality, without having to deal with `setoid` style
  objects that make for large Gallina terms. I don't fully understand how the
  _theory_ works: In particular, as far as I can tell, the synthetic interval
  type `I` allows one to only access the start and end points (`0` and `1`),
  but not anything in between, so I don't really see how it allows for
  interpolation.  I also don't understand how this allows us to make Univalence
  computable.  I feel I need to practice with this new technology before I'm
  well versed, but it's definitely a paper I'm going to read many, many times
  till I grok it.


- [Naive Cubical type theory](https://arxiv.org/abs/1911.05844). This paper
  promises a way to perform informal reasoning with cubical type theory, the
  way we are able to do so with, say, a polymorphic type theory for lambda
  calculus. The section names such as "how do we think of paths",
  "what can we do with paths", inspire confidence

- [Call by need is Clairvoyant call by value](https://icfp19.sigplan.org/details/icfp-2019-papers/26/Call-By-Need-is-Clairvoyant-Call-By-Value). This key insight is to notice that call by need
  is "just" call by value, when we evaluate only those values that are
  eventually forced, and throw away the rest. Thus, if we had an oracle that
  tells us which values are eventually forced, we can convert call by need into
  call by value, relative to this oracle. This cleans up many proofs in the
  literature, and might make it far more intuitive to teach call by need to
  people as well. Slick paper, I personally really enjoyed reading this.

- [Shift/Reset the Penultimate Backpropagator](https://arxiv.org/abs/1803.10228)
  This paper describes how to implement backprop using delimited continuations.
  Also, supposedly, using staging / building a compiler out of this paradigm
  allows one to write high performance compilers for backprop without having
  to suffer, which is always nice.


- [Closed forms for numerical loops](https://www.cs.princeton.edu/~zkincaid/pub/popl19a.pdf)
  This paper introduces a new algebra of polynomials with exponentials. It then
  studies the eigenvalues of the matrix that describes the loop, and tries to
  find closed forms in terms of polynomials and exponentials. They choose
  to only work with rationals, but not extensions of rational numbers
  (in terms of field extensions of the rationals). Supposedly, this is easier
  to implement and reason about. Once again, this is a paper I'd like to
  reimplement to understand fully, but the paper is well-done!


- [Composable, sound transformations of Nested recursion and loops](https://engineering.purdue.edu/Papers/Sundararajah.pdf). 
  This paper attempts to bring ideas from polyhedral compilation
  into working with nested recursion. They create a representation using
  multitape finite automata, using which they provide a representation for
  nested recursion. I was somewhat disappointed that it does not handle
  mutual recursion, since my current understanding is that one can always
  convert nested recursion into a "reasonable" straight line program by 
  simply inlining calls and then re-using polyhedral techniques. 


- [Reimplementation of `STOKE` at `bollu/blaze`.](https://github.com/bollu/blaze/blob/master/notebooks/tutorial.ipynb)
  I reimplemented the [STOKE: stochastic superoptimisation](http://stoke.stanford.edu/)
  paper, and much to my delight, it was super-effective at regenerating common
  compiler transformations. I want to use this to generate loop optimisations
  as well, by transforming a polyhedral model of the original program.
  

#### Internship at [Tweag.io](http://tweag.io/) over the summer: Hacking on Asterius (Haskell -> WebAssembly compiler)

- [Blog post on the progress made by me hacking on Austerius over at Tweag](https://www.tweag.io/posts/2019-09-12-webassembly-internship.html)

I really enjoyed my time at Tweag! It was fun, and
[Shao Cheng](https://github.com/TerrorJack)
was a great mentor. I must admit that I was somewhat distracted, by all the new
and shiny things I was learning thanks to all the cool people there `:)` In
particular, I wound up bugging 
[Arnaud Spiwack](http://assert-false.net/arnaud/), 
[Simeon Carstens](http://simeon-carstens.com/),
and [Matthias Meschede](https://github.com/mmesch)
quite a bit, about type theory, MCMC sampling, and signal processing of storm
clouds.

I wound up reading a decent chunk of GHC source code, and while I can't link
to specifics here, I understood a lot of the RTS much better than I did before.
It was an enlightening experience, to say the least, and being paid to hack on
a GHC backend was a really fun way to spend the summer.

It also led me to fun discoveries, such as
[how does one debug debug info?](https://github.com/ghc/ghc/blob/535a26c90f458801aeb1e941a3f541200d171e8f/compiler/cmm/Debug.hs#L458)


I also really loved Paris as a city. My AirBnb host was a charming artist who
suggest spots for me around the city, which I really appreciated. Getting
around was disorienting for the first week or so, due to the fact that I could
not (and still do not) really understand how to decide in which direction to
walk inside the subways to find a particular line _going in a particular
direction_.

The city has some great spots for quiet work, though! In particular, the
[Louvre Anticafe](https://www.anticafe.eu/lieux/louvre-paris-75001/)
was a really nice place to hang out and grab coffee. The model is great: you
pay for hours spent at the Anticafe, with coffee and snacks free. They also
had a discount for students which I gratefully used. 
I bumped into interesting artists, programmers, and students who were open for
conversation there. I highly recommend hanging out there. 

#### Probabilistic programming & giving a talk at FunctionalConf

This was the first talk I'd ever given, and it was on probabilistic programming
in haskell. In particular, I explained the
[`monad-bayes`](https://github.com/adscib/monad-bayes) approach of
doing this, and why this was profitable. 
[The slides are available here](https://github.com/bollu/functionalconf-2019-slides-probabilistic-programming/blob/master/slides.pdf).


It was a fun experience giving a talk, and I'd like to do more of it, since I
got a lot out of attempting to explain the ideas to people. I wish I had more
time, and had a clearer idea of who the audience was. I got quite a bit of
help from [Michael Snoyman](https://www.snoyman.com/) to whip the talk into
shape, which I greatly appreciated.

The major ideas of probabilistic programming as I described it are
from Adam Scibior's thesis:

- [Adam Scibior: Formally justified and modular Bayesian inference for probabilistic programs](https://www.cs.ubc.ca/~ascibior/assets/pdf/thesis.pdf)

Along the way, I and others at tweag read the other major papers in the space,
including:

- [Church, a language for generative models](https://arxiv.org/pdf/1206.3255), 
  which is nice since it describes it's semantics in terms of sampling. This is
  unlike Adam's thesis, where they define the denotational semantics in terms
  of measure theory, which is then approximated by sampling.


- [Riemann Manifold Langevin and Hamiltonian Monte Carlo](https://pdfs.semanticscholar.org/16c5/06c5bb253f7528ddcc80c72673fabf584f32.pdf)
  which describes how to perform Hamiltonian Monte Carlo on the _information
  geometry_ manifold.  So, for example, if we are trying to sample from
  gaussians, we sample from a 2D Riemannian manifold with parameters mean and
  varince, and metric as the [Fisher information metric](https://en.wikipedia.org/wiki/Fisher_information_metric).
  This is philosophically the "correct" manifold to sample from, since it
  represents the intrinsic geometry of the space we want to sample from.

- [An elementary introduction to Information geometry by Frank Nielsen](https://arxiv.org/pdf/1808.08271.pdf)
  something I stumbled onto as I continued reading about sampling from
  distributions. The above description about the "correct" manifold for
  gaussians comes from this branch of math, but generalises it quite a bit
  further. I've tried to reread it several times as I gradually gained maturity
  in differential geometry. I can't say I understand it just yet, but I hope to
  do so in a couple of months. I need more time for sure to meditate on the
  objects.

- [Reimplementation of `monad-bayes`](https://github.com/bollu/shakuni).
  This repo holds the original implementation on which the talk is based on.
  I read through the `monad-bayes` source code, and then re-implemented the
  bits I found interesting. It was a nice exercise, and you can see
  the git history tell a tale of my numerous mis-understandings of MCMC methods,
  till I finally got what the hell was going on.

#### Presburger Arithmetic

Since we use a bunch of [presburger arithmetic](https://en.wikipedia.org/wiki/Presburger_arithmetic)
for [polyhedral compilation](http://polyhedral.info/)
which is a large research interest of mine, I've been trying to build a
"complete" understanding of this space. So this time, I wanted to learn
how to build good solvers:

- [`bollu/gutenberger`](https://github.com/bollu/gutenberger) is a decision
  procedure for Presburger arithmetic that exploits their encoding as finite
  automata. One thing that I was experimenting with was that we only use
  numbers of finite bit-width, so we can explore the entire state space
  of the automata and then perform NFA reduction using
  [DFA minimisation](https://en.wikipedia.org/wiki/DFA_minimization). The
  reference I used for this was the excellent textbook
  [Automata theory: An algorithmic approach, Chapter 10](https://www7.in.tum.de/~esparza/autoskript.pdf)


- [The taming of the semi-linear set](http://www.lsv.fr/~haase/documents/ch16.pdf)
  This uses a different encoding of presburger sets, which allows them to bound
  a different quantity (the norm) rather than the bitwidth descriptions. This allows
  them to compute _exponentially_ better bounds for some operations than
  were known before, which is quite cool. This is a paper I keep trying to
  read and failing due to density. I should really find a week away from civilization
  to just plonk down and meditate upon this.
  
##### Open questions for which I want answers

I want better references to being able to _regenerate_ the inequalities
description from a given automata which accepts the presburger set automata.
This will allow one to smoothly switch between the _geometric_ description
and the _algebraic_ description. There are some operations that only work
well on the geometry (such as optimisation), and others that only work well on
the algebraic description (such as state-space minimisation). I have not found
any good results for this, only scattered fragments of partial results.
If nothing else, I would like some kind of intuition for _why this is hard_. 

Having tried my stab at it, the general impression that I have is that the
space of automata is much larger than the things that can be encoded as
presburger sets. Indeed, it was shown that automata accept numbers which
are ultimately periodic. 

-  first order logic + "arithmetic with +" + (_another operation I cannot recall_).
   I'm going to fill this in once I re-find the reference.

But yes, it's known that automata accept a language that's broader than just
first order logic + "arithmetic with +", which means it's hard to dis-entangle
the presburger gits from the non-presburger bits of the automata. 
  
  
#### Prolog

I wanted to get a better understading of how prolog works under the hood, so I began
re-implementing the [WAM: warren abstract machine](http://wambook.sourceforge.net/).
It's really weird, this is the _only stable reference_ I can find to implementing
high-performance prolog interpreters. I don't really understand how to chase the
paper-trail in this space, I'd greatly appreciate references. My implementation
is at [`bollu/warren-cpp`](https://github.com/bollu/warren-cpp/). Unfortunately,
I had to give up due to a really hard-to-debug bug.

It's crazy to debug this abstract machine, since the internal representation gets
_super convoluted_ and hard to track, due to the kind of optimised encoding it
uses on the heap.

If anyone has a better/cleaner design for implementing good prologs, I'd love
to know.

Another fun paper I found in this space thanks to Edward Kmett was
[the Rete matching algorithm](http://www.drdobbs.com/architecture-and-design/the-rete-matching-algorithm/184405218),
which allows one to declare many many pattern matches, which are then "fused"
together into an optimal matcher that tries to reuse work across failed
matchers.

#### General Relativity

This was on my "list of things I want to understand before I die", so I wound
up taking up an Independent Study in university, which basically means that
I study something on my own, and visit a professor once every couple weeks,
and am graded at the end of the term. For GR, I wound up referencing a wide
variety of sources, as well as a bunch of pure math diffgeo books. I've read
everything referenced to various levels. I feel I did take away the core
ideas of differential and Riemannian geometry. I'm much less sure I've grokked
general relativity, but I can at least read the equations and I know all the
terms, so that's something.

- [The theoretical minimum by Leonard Susskind](https://theoreticalminimum.com/courses/general-relativity/2012/fall).
  The lectures are breezy in style, building up the minimal theory (and no proofs)
  for the math, and a bunch of lectures spent analysing the physics. While I wish
  it were a little more proof heavy, it was a really great reference to learn the
  basic theory! I definitely recommend following this and then reading other
  books to fill in the gaps.

- [Gravitation by Misner Thorne and Wheeler](https://en.wikipedia.org/wiki/Gravitation_(book))
  This is an imposing book. I first read through the entire thing (Well, the parts I thought I needed),
  to be able to get a vague sense of what they're going for. They're rigorous in
  a very curious way: It has a bunch of great _physics_ perspectives of looking
  at things, and that was invaluable to me. Their view of forms as "slot machines"
  is also fun. In general, I found myself repeatedly consulting this book for
  the "true physical" meaning of a thing, such as curvature, parallel transport,
  the equation of a geodesic, and whatnot.

- [Differential Geometry of Curves and Surfaces by do Carmo](http://www2.ing.unipi.it/griff/files/dC.pdf)
  This is the best book to intro differential geometry I found. It throws away
  all of the high powered definitions that "modern" treatments offer, and
  starts from the ground up, building up the theory in 2D and 3D. This is amazing,
  since it gives you small, computable examples for things like
  "the Jacobian represents how tangents on a surface are transformed locally".

- [Symplectic geometry & classical mechanics by Tobias Osborne](https://www.youtube.com/watch?v=pXGTevGJ01o&list=PLDfPUNusx1EoVnrQcCRishydtNBYU6A0c)
  This lecture series was great, since it re-did a lot of the math I'd seen
  in a more physicist style, especially around vector fields, flows, and
  Lie brackets. Unfortunately for me, I never even _got_ to the classical
  mechanics part by the time the semester ended. I began 
  [taking down notes in my repo](https://github.com/bollu/notes/blob/master/diffgeo/main.pdf), 
  which I plan to complete.

- [Introduction to Smooth manifolds: John Lee](https://sites.math.washington.edu/~lee/Books/ISM/)
  This was a very well written _mathematical_ introduction to differential geometry. 
  So it gets to the physically important bits (metrics, covariant derivatives)
  far later, so I mostly used it as a reference for problems and more rigour.


- [Einstein's original paper introducing GR, translated](http://hermes.ffn.ub.es/luisnavarro/nuevo_maletin/Einstein_GRelativity_1916.pdf)
  finally made it click as to _why_
  he wanted to use tensor equations: tensor equations of the form `T = 0` are
  invariant in _any coordinate system_, since on change of coordinates, `T`
  changes by a multiplicative factor! It's a small thing in hindsight, but it
  was nice to see it explicitly spelled out, since as I understand, no one
  among the physicists knew tensor calculus at the time, so he had to introduce
  all of it.

#### Discrete differential geometry

I can't recall how I ran across this: I think it was because I was trying to
get a better understanding of Cohomology, which led me to Google for 
"computational differential geometry", that finally led me to Discrete
differential geometry.

It's a really nice collection of theories that show us how to discretize
differential geometry in low dimensions, leading to rich intuitions and
a myriad of applications for computer graphics.

- [The textbook by Kennan Crane on the topic](https://www.cs.cmu.edu/~kmcrane/Projects/DDG/paper.pdf) 
  which I read over the summer when I was stuck (more often than I'd like) in
  the Paris metro. The book is very accessible, and requires just some
  imagination to grok. Discretizing differential geometry leads to most things
  being linear algebra, which means one can calculate things on paper easily.
  That's such a blessing.

- [Geodesics in Heat](https://arxiv.org/pdf/1204.6216)
  explores a really nice way to discover geodesics by simulating the heat
  equation for a short time. The intuition is that we should think of the heat
  equation as describing the evolution of particles that are performing random
  walks. Now, if we simulate this system for a short while and then look at the
  distribution, particles that reach a particular location on the graph _must
  have taken the shortest path_, since any longer path would not have allowed
  particles to reach there. Thus, the distribution of particles at time `dt`
  does truly represent distances from a given point.  The paper explores this
  analogy to find accurate geodesics on complex computational grids. This is
  aided by the use of differential geometry, appropriately discretized.

- [The vector heat method](https://arxiv.org/pdf/1805.09170.pdf)
  explores computing the parallel transport of a vector across a discrete
  manifold efficiently, borrowing techniques from the 'Geodesics in Heat'
  paper.

- [Another paper by Kennan Crane: Lie group integrators for animation and control of vehicles](https://www.cs.cmu.edu/~kmcrane/Projects/LieGroupIntegrators/paper.pdf)
  This paper describes a general recipe to tailor-make integrators for a system
  of constraints, by directly integrating over the lie group of the
  configuration space.  This leads to much more stable integrators. I have some
  misguided hope that we can perhaps adapt these techniques to build better FRP
  (functional reactive programming) systems, but I need to meditate on this a
  lot more to say anything definitively.

  
  

#### Synthetic differential geometry

It was [Arnaud Spiwack](http://assert-false.net/arnaud/)
who pointed me to this. It's a nice axiomatic
system of differential geometry, where we can use physicist style proofs of
"taking stuff upto order `dx`", and having everything work upto mathematical
rigor.

The TL;DR is that we want to add a new number called `dx` into the reals,
such that `dx^2 = 0`. But if such a number did exist, then clearly `dx = 0`.
However, the punchline is that to prove that `dx^2 = 0 => dx = 0` requires
the use of contradiction!

So, if we banish the law of excluded middle (and therefore no longer use
proof by contradiction), we are able to postulate the existence of a new
element `dx`, which obeys `dx^2 = 0`. Using this, we can build up the
whole theory of differential geometry in a pleasing way, without having to
go through the horror that is real analysis. (I am being hyperbolic, but really,
real analytic proofs are not pleasant).

[I began formalizing this in Coq and got a formalism going: `bollu/diffgeo`](https://www.github.com/bollu/diffgeo).

Once I was done with that, I realised I don't know how to exhibit _models_ of
the damn thing! So, reading up on that made me realise that I need around 8
chapters worth of a grad level textbook (the aptly named 
[Models of Smooth Infinitesimal Analysis](https://link.springer.com/book/10.1007/978-1-4757-4143-8)).

I was disheartened, so I [asked on `MathOverflow`](https://mathoverflow.net/questions/346385/constructing-computable-synthetic-differential-geometry)
(also my first ever question there), where I learnt about tangent categories and
differential lambda calculus. Unfortunately, I don't have the bandwidth to read
another 150-page tome, so this has languished.


### Optimisation on Manifolds

I began reading
[Absil: Optimisation on matrix manifolds](http://www.eeci-institute.eu/GSC2011/Photos-EECI/EECI-GSC-2011-M5/book_AMS.pdf)
which describes how to perform optimisation / gradient descent on 
_arbitrary Riemannian manifolds_, as well as closed forms for well-known
manifolds. The exposition in this book is really good, since it picks a
concrete manifold and churns out all the basic properties of it manually. The
only problem I had with the books was that there were quite a few gaps (?) in
the proofs -- perhaps I missed a bunch.

This led me to learn Lie theory to some degree, since that was the natural
setting for many of the proofs. I finally saw _why_ anyone gives a shit about
the tangent space at the identity: because it's _easier to compute!_ For a
flavour
of this, [consider this question on `math.se` by me that asks about computing
tangent spaces of $O(n)$](https://math.stackexchange.com/questions/3389983/explicit-description-of-tangent-spaces-of-on).

#### AIRCS workshop

I attended the
[AI risk for computer scientists](https://intelligence.org/ai-risk-for-computer-scientists/)
workshop hosted by
[MIRI (Machine intelligence research institute)](https://intelligence.org/) in 
December. Here, a bunch of people were housed at a bed & breakfast for a
week, and we discussed AI risk, why it's potentially the most important thing
to work on, and anything our hearts desired, really. I came away with new
branches of math I wanted to read, a better appreciation of the AI risk
community and a sense of what their "risk timelines" were, and some
explanations about sheaves and number theory that I was sorely lacking. All in
all, it was a great time, and I'd love to go back. 

#### P-adic numbers

While I was on a particularly rough flight back from the USA to India when
coming back from the AIRCS workshop, I began to read the textbook
[Introduction to p-adic numbers by Fernando Gouvea](https://www.springer.com/gp/book/9783540629115),
which fascinated me, so I then
[wrote up the cool parts introduced in the first two chapters as a blog post](http://bollu.github.io/#a-motivation-for-p-adic-analysis).
I wish to learn more about the p-adics and p-adic analysis, since they
seem to be deep objects in number theory.

In particular, a question that I thought might have a somewhat trivial answer
([why do the p-adics use base p in defining norm](https://math.stackexchange.com/questions/3482489/why-does-the-p-adic-norm-use-base-p))
turned out to have answers that were quite deep, which was something
unexpected and joyful!

#### Topology of functional programs

- [Slides by Martın Escardo](http://cs.ioc.ee/ewscs/2012/escardo/slides.pdf)
- [Synthetic topology of data types and classical spaces](https://pdf.sciencedirectassets.com/272990/1-s2.0-S1571066104X00177/1-s2.0-S1571066104051357/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEJP%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJHMEUCIGQAb828p1io4csznEej60j0PwJteuXf7OoHLSCDhkUTAiEA9ITs1JrUEOE%2Ft%2Fl5TI9ZkNLUfBIx42IZ%2FoAqQpdX4twq2AII6%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FARACGgwwNTkwMDM1NDY4NjUiDBQOxJ3s3HCbVxSheCqsAt65yorZMMtIhLF7ML5sJQ9S5wZxBayKDrRkUKOjSzXxtQWebXs70FXhhpToXKvJoDrLgsqDzdF%2FAshZY%2FkDUep6KILxKnYxCBrBINhjFqxDlRZH0s1Y991RgCyNNnwmFI%2BrH70lSrV0OxQ9Z5WdXXPDkTLXv8512Xz%2BJn5DqEqdqD49FLbOuHl6PRM0TnYyNJkLBXNVwt75kkGkaTfdgmgiqh7YpcXcHlbqI2oqNcaxFDewXwKDCC7qWD6ECclLgszoeOXOtRI91nvNac8%2BLV4bXkKLXpd99H94N2vXPUPz99p6oqfdY9ixtcfI9POFX8agUilYjXKVhAWk4FSzzzMqbtZLBfkCZT4ffDTxRgL52yD%2FmL5E0Pe4mczVlUoB5DKoB8Lkitrt0BumQzDr4ffvBTrQAjVRuzG5V0CC%2Fd1t%2BUMPkrywaYytbrXCZ%2BkDo0xDBqsljY8DaGIiFINr8BEEpT7UX42GRhcDzpnOnztdAOTea3qZ3SmXJwgEoh0aiz%2B87MmsC57s0Q%2F%2B%2FDDvHBY3zLCrz7rdewXOgk6VxI9d5mhG3Du1dwPRbgOe798S2waDCD8LQA3rw7w5wNGa9Uv3xtNVH%2BHw%2FXcQ6OiubO4GL9mK8U5g7TVPh1hLB26XBQooKJ564VGf4J9VqWxjlx3NicVhqnFlGevNJNKyVLiyRsRCyQGMV59%2BXqUwdEMQZYWLbfUwELNz1NKfWumvu9BXC5jjsJgNx%2FRERSb7hqT1svMJU91o%2FHtatGAnPvVjYaNthha9O9jm%2BG9nw1vMsdyJ0asI5w5SrlsEyb5C7Vk7aLBcHAEi3XPRhivY1Q4hZAN0xY9VfEZrF%2FoM9HCGxr5cYs%2BP9w%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20191221T113658Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTY6QRXTPOC%2F20191221%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=8a26656e8f8c8772afdc2ae6caa9b583fd59a8ee6303d9770d25b4a3d8a6291f&hash=42518d3e5cc1e77ed961c359d0ea8f59bd582d1e5f13d69c3eeb2563a6c82abd&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S1571066104051357&tid=spdf-584e8188-4806-44bb-a118-9b63a53caaff&sid=f53691052392834d3e18a62484e2909639a9gxrqb&type=client)

Both of these describe a general method to transfer all topological ideas as
statements about _computability_ in a way that's far more natural (at least for
me, as a computer scientist). The notion of what a continuous function "should
be" (keeping inverse images of open sets open) arises naturally from this
computational viewpoint, and many proofs of topology amount to finding
functional programs that fit a certain type. It's a great read, and I feel gave
me a much better sense of what topology is trying to do.

- [I've written some exposition on this topic](http://bollu.github.io/#topology-is-really-about-computation--part-1),
  which maybe a more accessible read than the links above, since it tries to
  distill the fundamental idea into a blog post.

#### Philosophy

I've wanted to understand philosophy as a whole for a while now, at least
enough to get a general sense of what happened in each century. The last year,
I meandered through some philosophy of science, which led me to some really
wild ideas (such as that of
[Paul Feyerabend's 'science as an anarchic enterprise'](https://en.wikipedia.org/wiki/Epistemological_anarchism) 
which I really enjoyed).

I also seem to get a lot more out of audio and video than text in general, so
I've been looking for podcasts and video lectures. I've been following:

- [The history of philosophy without any gaps](https://historyofphilosophy.net/)
  for a detailed exposition on, say, the greeks, or the arabic philosophers.
  Unfortunately, this podcast focuses on far too much detail for me to have been
  able to use it as a way to get a broad idea about _philosophy_ in itself.

- [Philosophize This! by Stephen West](http://philosophizethis.org/)
  Is a good philosophy podcast for a _broad_ overview of different areas
  of Philosophy. I got a lot out of this, since I was able to get a sense
  of the progression of ideas in (Western) Philosophy. So I now know what
  [Phenomenology](https://plato.stanford.edu/entries/phenomenology/) is, 
  or what Foucault was reacting against.


I also attempted to read a bunch of philosophers, but the only ones I could
make a good dent on were the ones listed below. I struggled in writing this
section, since it's much harder to sanity check my understanding of philosophy,
versus mathematics, since there seems to be a range of interpretations of the
same philosophical work, and the general imprecise nature of language doesn't
help here at all. So please take all the descriptions below with some salt 
to taste.

- [Discipline and Punish by Michel Foucault](https://en.wikipedia.org/wiki/Discipline_and_Punish)
  Here, Foucault traces the history of the criminal justice system of society,
  and how it began as something performed 'on the body' (punishment),
  which was then expanded to a control 'of the mind' (reform). As usual,
  the perspective is fun, and I'm still going through the book.

- [Madness and Civilization by Michel Foucault](https://en.wikipedia.org/wiki/Madness_and_Civilization)
  which attempts to chronicle how our view of madness evolved as society did.
  It describes how madmen, who were on the edges of society, but still
  "respected" (for exmaple, considered as 'being touched by the gods') were
  pathologized by the Renaissance, and were seen as requiring treatment. I'm
  still reading it, but it's enjoyable so far, as a new perspective for me.

  
- [The value of science by Henri Poincare](https://en.wikipedia.org/wiki/The_Value_of_Science).
  Here, he defends the importance of experimentation, as well as the value of
  intuition to mathematics, along with the importance of what we consider
  formal logic. It's a tough read sometimes, but I think I got something out of
  it, at least in terms of perspective about science and mathematics.

#### Information theory

I've been on a quest to understand information theory far better than I
currently do. In general, I feel like this might be a much better way to
internalize probability theory, since it feels like it states probabilistic
objects in terms of "couting" / "optimisation of encodings", which is a 
perspective I find far more natural.

Towards this aim, I wound up reading:

- [Information theory, Learning, and inference algorithms](http://www.inference.org.uk/mackay/itila/book.html)
  This book attempts to provide the holistic view I was hoping for. It has
  great illustrations of the basic objects of information theory. However,
  I was hoping that the three topics would be more "unified" in the book,
  rather than being presented as three separate sections with some amount
  of back-and-forth-referencing among them. Even so, it was a really fun read.

- [Elements of information theory](http://www.cs-114.org/wp-content/uploads/2015/01/Elements_of_Information_Theory_Elements.pdf)

#### Building intuition for Sheaves, Topoi, Logic

I don't understand the trifecta of sheaves, topoi, geometry, and logic, and
I'm trying to attack this knot from multiple sides at once.

- [Understanding networks and their behaviours using sheaf theory](https://arxiv.org/pdf/1308.4621)
- [Sheaves, objects, and distributed systems](https://www.sciencedirect.com/science/article/pii/S1571066108005264)
- [Sheaves are the canonical data strucure for sensor integration](https://arxiv.org/pdf/1603.01446.pdf)
- [Elementary applied topology, Chapter 9: Sheaves](https://www.math.upenn.edu/~ghrist/EAT/EATchapter9.pdf)
- [Sheaf theory: The mathematics of data fusion (video) (link to HackerNews)](https://news.ycombinator.com/item?id=13677308)

All of these provide geometric viewpoints of what sheaves are, in low-dimensional
examples of graphs which are easy to visualize. I'm also trudging through the
tome:

- [Sheaves in geometry and logic: A first introduction to Topos theory](https://www.springer.com/gp/book/9780387977102)

which appears to follow the "correct path" of the algebraic geometers, but this
requires a lot of bandwidth.

- [The Rising Sea: foundations of algebraic geometry by Ravi Vakil](http://math.stanford.edu/~vakil/216blog/FOAGnov1817public.pdf)

This is a hardcore algebraic geometry textbook, and is arguably 
_great for studying sheaves_ because of it. Sheaves are Chapter 2, and allows
one to see them be developed in their "true setting" as it were. In that
Grothendeick first invented sheaves for algebraic geometry, so it's good to
see them in the home they were born in. Once again, this is a book I lack
bandwidth for except to breezily read it as I go to bed. I did get something
out from doing this. I'm considering taking this book up as an independent
study, say the first four chapters. I'll need someone who knows algebraic
geometry to supervise me, though, which is hard to find in an institute geared
purely for computer science. (If anyone on the internet is kind enough to
volunteer some of their time to answer questions, I'll be very glad! Please
email me at `rot13(fvqqh.qehvq@tznvy.pbz)`)

#### The attic

This section contains random assortments that I don't recall how I stumbled
across, but too cool to not include on the list. I usually read these in bits
and pieces, or as bedtime reading right before I go to bed to skim.  I find
that skimming such things gives me access to knowing about tools I would not
have known otherwise. I like knowing the existence of things, even if I don't
recall the exact thing, since knowing that something like `X` exists has saved me
from having to reinvent `X` from scratch.

- [Group Theory: Birdtracks, Lie's and Exceptional Groups by Predrag Cvitanovic](http://www.cns.gatech.edu/GroupTheory/version9.0/GroupTheory.pdf)
  is an exposition of Lie theory using some notation called as "Birdtrack notation",
  which is supposedly a very clean way of computing invariants, inspired by
  Feynmann notation. The writing style is informal and pleasant, and I decided
  to save the book purely because the first chapter begins with
  "Basic Concepts: A typical quantum theory is constructed from a few building blocks...".
  If a book considers building quantum theories as its starting point, I really
  want to see where it goes.

- [Elementary Applied topology by Robert M Ghirst](https://www.math.upenn.edu/~ghrist/notes.html)
  I wouldn't classify the book as elementary because it skims over too much to be
  useful as a reference, but it's great to gain an intuition for what, say,
  homology or cohomology is. I am currently reading the section on Sheaf theory,
  and I'm getting a lot out of it, since it describes how to write down, say,
  min-cut-max-flow or niquist-shannon in terms of sheaves. I don't grok it yet,
  but even knowing this can be done is very nice. The book is a wonderful
  walkthrough in general.


- [On polysemous mathematical illustration by Robert M Ghirst](https://icerm.brown.edu/video_archive/?play=2034)
  This is a talk on the wonderful illustrations by the above author, about
  the different types of mathematical illustrations one can have, and different
  "levels of abstraction".

- [Mathematical Impressions: The illustrations of AT Femenko](http://chronologia.org/en/math_impressions/poster016.html)
  These are _beautiful_ illustrated pictures of various concepts in math, which
  tend to _evoke_ the feeling of the object, without being too direct about it.
  For example, consider "gradient descent" below. I highly recommend going
  through the full gallery.


- [Gradient Descent](http://chronologia.org/art/math/123a176.jpg)
  <img width=200 height=200 src="http://chronologia.org/art/math/123a176.jpg">

- [Topological Zoo](http://chronologia.org/art/math/077a011.jpg)
  <img width=200 height=200 src="http://chronologia.org/art/math/077a011.jpg">

- [Persistent Homology in Multivariate Data Visualization](https://bastian.rieck.me/research/Dissertation_Rieck_2017.pdf)
  This is the PhD dissertation of [Bastian Rieck](https://bastian.rieck.me/),
  who's now a postdoc at ETH. I deeply enjoyed reading it, since it pays
  a lot of attention to the _design_ of analyses, and how to interpret
  topological data. I really enjoyed getting a good sense of how one can 
  use persistent homology to understand data, and the trade-offs between
  [Vietoris-Rips complex](https://en.wikipedia.org/wiki/Vietoris%E2%80%93Rips_complex)
  and the [Cech complex](https://en.wikipedia.org/wiki/%C4%8Cech_complex).

- [An introduction to Geometric algebra](https://arxiv.org/pdf/1205.5935.pdf)
  I fell in love with geometric algebra, since it provides a really clean way
  to talk about _all possible subspaces_ of a given vector space. This provides
  super slick solutions to many geometry and linear algebra problems. The
  way I tend to look at it is that when one does linear algebra, there's a strict
  separation between "vectors" (which are elements of the vector space), and,
  say, "hyperplanes" (which are _subspaces_ of the vector space), as well as
  objects such as "rotations" (which are _operators_ on the vector space). 
  Geometric algebra provides a rich enough _instruction set_ to throw all
  these three distinct things into a blender. This gives a really concise
  language to describe all phenomena that occurs in the vector space world --- 
  which, let's be honest, is _most_ tractable phenomena! I had a blast
  reading about GA and the kinds of operators it provides.

- [Circuits via Topoi](https://arxiv.org/pdf/1807.07159). This paper attempts
  to provide an introduction to topos theory by providing a semantics for
  both combinational and sequential circuits under a unifying framework. I keep
  coming back to this article as I read more topos theory. Unfortunately, I'm
  not "there yet" in my understanding of topoi. I hope to be next year!


- [Fearless Symmetry](https://press.princeton.edu/books/paperback/9780691138718/fearless-symmetry)
  This is definitely my favourite non-fiction book that I've read in 2019, hands
  down. The book gives a great account of the mathematical objects that went
  into Wiles' book of Fermat's last theorem. It starts with things like
  "what is a permutation" and ends at questions like "what's a reciprocity law"
  or "what's the absolute galois group". While at points, I do believe the book
  goes far too rapidly, all in all, it's a solid account of number theory
  that's distilled, but not in any way diluted. I really recommend reading this
  book if you have any interest in number theory (or, like me, a passing
  distaste due to a course on elementary number theory I took, with proofs that
  looked very unmotivated). This book made me decide that I should, indeed, 
  definitely learn algebraic number theory, upto at least
  [Artin Reciprocity](https://en.wikipedia.org/wiki/Artin_reciprocity_law).


- [Rememberance of Earth's past trilogy by Liu Cixin](https://en.wikipedia.org/wiki/Remembrance_of_Earth%27s_Past)
  While I would not classify this as "mind-blowing" (which I do classify Greg 
  Egan books as), they were still a solidly fun read into how humanity would
  evolve and interact with alien races. It also poses some standard solutions
  to the Fermi Paradox, but it's done well. I felt that the fact that it was
  translated was painfully obvious in certain parts of the translation, which
  I found quite unfortunate. However, I think book 3 makes up in grandeur for
  whatever was lost in translation.

- [Walkaway by Cory Doctorow](https://en.wikipedia.org/wiki/Walkaway_(Doctorow_novel))
  The book is set in a dystopian nightmare, where people are attempting to
  "walk away" from society and set up communes, where they espouse having
  a post-scarcity style economy based on gifting. It was a really great
  description of what such a society could look like. I took issue with some
  weird love-triangle-like-shenanigans in the second half of the book, but
  the story arc more than makes up for it. Plus, the people throw a party
  called as a "communist party" in the first page of the book, which grabbed
  my attention immediately!

- [PURRS: Parma University Recurrence Relation Solver](http://www.cs.unipr.it/purrs/)
  I wanted better tactics for solving recurrences in Coq, which led me into
  a rabbit hole of the technology of recurrence relation solving. This was the
  newest _stable_ reference to a complete tool that I was able to find. Their
  references section is invaluable, since it's been vetted by them
  actually implementing this tool!

- [Term rewriting and all that](https://www21.in.tum.de/~nipkow/TRaAT/).
  I read this book purely for its description of Groebner bases and the Bucchberger
  algorithm in a way that _made sense_ for the first time.
  [I've written about this more extensively before](http://bollu.github.io/#what-the-hell-is-a-grobner-basis-ideals-as-rewrite-systems)
  so I'm not going to repeat myself here. In general, I think it's a great book
  that's worth reading, if nothing else, for at least the chapter on Groebner
  bases.

- [Lucid: The dataflow programming language](http://worrydream.com/refs/Wadge%20-%20Lucid,%20the%20Dataflow%20Programming%20Language.pdf)
  This document is the user manual of Lucid. I didn't fully understand the
  book, but what I understood as their main argument is that full access too
  looping is un-necessary to perform most of the tasks that we do. Rather,
  one can provide a "rich enough" set of combinators to manipulate streams
  that allows one to write all programs worthy of our interest.

- [Bundle Adjustment — A Modern Synthesis](https://hal.inria.fr/inria-00548290/document)
  I learnt about Bundle Adjustment from a friend taking a course on robotics.
  The general problem is to reconstruct the 3D coordinates of a point cloud
  given 2D projections of the points and the camera parameters, as the camera
  moves in time. I found the paper interesting since it winds up invoking a
  decent amount of differential geometric and gauge theoretic language to
  describe the problem at hand. I was unable to see why this vocabulary helped
  in this use-case, but perhaps I missed the point of the paper. It was hard to
  tell.

#### Conclusions

I always feel a little wrong posting this at the end of every year, since I
feel that among the things I cover under "read", I've internalized some things
far better than others: For example, I feel I understannd Riemannian geometry
far better than I do General Relativity. I try to put up the caveats at the
beginning of each section, but I'd really like a way to communicate my
confidence without reducing readability.


The final thing that I wish for is some kind of reading group? It's hard
to maintain a group when my interests shift as rapidly as they do, which
was one of the reason I really loved the AIRCS workshop: They were people
who were working on formal methods, compilers, type theory, number theory,
embedded systems, temporal logic... It was very cool to be in a group of
people who had answers and intuitions to questions that had bugged me for
some time now. I wonder if attending courses at a larger research university
feels the same way. My uni is good, but we have quite small population, which
almost by construction means reduced diversity.

I also wish that I could openly add more references to repos I've been working
on for a while now, but I can't due to the nature of academia and publishing.
This one bums me out, since there's a long story of a huge number of commits
and trial-by-fire that I think I'll be too exhausted to write about once the
thing is done. 

Sometimes, I also wish that I could spend the time I spend reading _disparate_
topics on _focused reading on one topic_. Unfortunately, I feel like I'm not
wired this way, and the joy I get from sampling many things at the same time
and making connections is somehow much deeper than the joy I get by deeply
reading one topic (in exclusion of all else). I don't know what this says
about my chances as a grad student in the future `:)`.


# [A motivation for p-adic analysis](#a-motivation-for-p-adic-analysis)

I've seen the definitions of p-adic numbers scattered around on the internet,
but this analogy as motivated by the book 
[p-adic numbers by Fernando Gouvea](https://www.springer.com/gp/book/9783540629115) 
really made me understand why one would study the p-adics, and why the
definitions are natural. So I'm going to recapitulate the material, with the
aim of having somoene who reads this post be left with a sense of why it's
profitable to study the p-adics, and what sorts of analogies are fruitful when
thinking about them.

We wish to draw an analogy between the ring $\mathbb C[X]$, where $(X - \alpha)$
are the prime ideals, and $\mathbb Z$ where $(p)$ are the prime ideals. We wish
to take all operations one can perform with polynomials, such as generating
functions ($1/(X - \alpha) = 1 + X + X^2 + \dots$ ), 
taylor expansions (expanding aronund $(X - \alpha)$),
and see what their analogous objects will look like in $\mathbb Z$
relative to a prime $p$.

#### Perspective: Taylor series as writing in base $p$:

Now, for example, given a prime $p$, we can write any positive integer $m$
in base $p$, as $(m = \sum_{i=0}^n a_i p^i)$ where $(0 \leq a_i \leq p - 1)$.

For example, consider $m = 72, p = 3$. The expansion of 72 is 
$72 = 0\times 1 + 0 \times 3 + 2 \times 3^2 + 2 \times 3^3$.
This shows us that 72 is divisible by $3^2$.

This perspective to take is that this us the information local to prime $p$,
about what order the number $m$ is divisible by $p$,
just as the taylor expansion tells us around $(X - \alpha)$ of a polynomial $P(X)$ 
tells us to what order $P(X)$ vanishes at a point $\alpha$.

#### Perspective: rational numbers and rational functions as infinite series:

Now, we investigate the behaviour of expressions such as 
- $P(X) = 1/(1+X) = 1 - X + X^2 -X^3 + \dots$.

We know that the above formula is correct formally from the theory of
generating functions.  Hence, we take inspiration to define values for
_rational numbers_.

Let's take $p \equiv 3$, and we know that $4 = 1 + 3 = 1 + p$.

We now calculate $1/4$ as:

$$
1/4 = 1/(1+p) = 1 - p + p^2 - p^3 + p^4 - p^5 + p^6 + \cdots
$$

However, we don't really know how to interpret $(-1 \cdot p)$, since we assumed
the coefficients are always non-negative. What we can do is to rewrite $p^2 = 3p$,
and then use this to make the coefficient positive. Performing this transformation
for every negative coefficient, we arrive at:


$$
\begin{align*}
1/4 &= 1/(1+p) = 1 - p + p^2 - p^3 + p^4 + \cdots \\
&= 1 + (- p + 3p) + (- p^3 + 3p^3)  +  \cdots \\
&= 1 + 2p + 2p^3 + \cdots
\end{align*}
$$

We can verify that this is indeed correct, by multiplying with $4 = (1 + p)$
and checking that the result is $1$:

$$
\begin{align*}
&(1 + p)(1 + 2p + 2p^3 + \cdots) \\
&= (1 + p) + (2p + 2p^2) + (2p^3 + 2p^4) + \cdots \\
&= 1 + 3p + 2p^2 + 2p^3 + 2p^4 + \cdots \\
&\text{(Rewrite $3p = p \cdot p = p^2$)} \\
&= 1 + (p^2 + 2p^2) + 2p^3 + 2p^4 + \cdots \\
&= 1 + 3p^2 + 2p^3 + 2p^4 + \cdots \\
&\text{(Rewrite $3p^2 = p^3$ and collect $p^3$)} \\
&= 1 + 3p^3 + 2p^4 + \cdots \\
&= 1 + 3p^4 + \cdots \\
&= 1 + \cdots = 1
\end{align*}
$$

What winds up happening is that all the numbers after $1$ end up being cleared
due to the carrying of $(3p^i \mapsto p^{i+1})$.

This little calculation indicates that we can also define take the $p$-adic
expansion of _rational numbers_.

#### Perspective: -1 as a p-adic number

We next want to find a p-adic expansion of -1, since we can then expand
out theory to work out "in general". The core idea is to "borrow" $p$, so
that we can write -1 as $(p - 1) - p$, and then we fix $-p$, just like we fixed
$-1$. This eventually leads us to an infinite series expansion for $-1$. Written
down formally, the calculation proceeds as:

$$
\begin{align*}
-1 &= -1 + p - p  \qquad \text{(borrow $p$, and subtract to keep equality)} \\
&= (p - 1) - p \qquad \text{(Now we have a problem of $-p$)} \\
&= (p - 1) - p + p^2 - p^2  \\
&= (p - 1) + p(p - 1) - p^2 \\
&= (p - 1) + p(p - 1) - p^2 + p^3 - p^3 \\
&= (p - 1) + p(p - 1) + p^2(p - 1) - p^3 \\
&\text{(Generalizing the above pattern)} \\
-1 &= (p - 1) + p(p - 1) + p^2(p - 1) + p^3(p - 1) + p^4(p - 1) + \cdots \\
\end{align*}
$$

This now gives us access to negative numbers, since we can formally multiply
the series of two numbers, to write $-a = -1 \cdot a$. 


Notice that this definition of $-1$ also curiously matches the 2s complement 
definition, where we have $-1 = 11\dots 1$. In this case, the expansion is
_infinite_, while in the 2s complement case, it is finite. I would be very
interested to explore this connection more fully.

#### What have we achieved so far?

We've now managed to completely reinterpret all the numbers we care about in 
the rationals as power series in base $p$. This is pretty neat. We're next
going to try to _complete_ this, just as we complete the rationals to get
the reals. We're going to show that we get a _different_ number system on
completion, called $\mathbb Q_p$. 

To perform this, we first look at how the $p$-adic numbers help us solve
congruences mod p, and how this gives rise to completions to equations such
as $x^2 - 2 = 0$, which in the reals give us $x = \sqrt 2$, and in $\mathbb Q_p$
give us a different answer!

#### Solving $X^2 \equiv 25 \mod 3^n$

Let's start by solving an equation we already know how to solve:
$X^2 \equiv 25 \mod 3^n$. 

We already know the solutions to $X^2 \equiv 25 \mod 3^n$ in $\mathbb Z$ are 
$X \equiv \pm 5 \mod 3^n$.

Explicitly, the solutions are:
- $X \equiv 3 \mod 3$
- $X \equiv 5 \mod 9$
- $X \equiv 5 \mod 27$
- At this point, the answer remains constant.

This was somewhat predictable. We move to a slightly more interesting case.

#### Solving $X = -5 \mod 3^n$

The solution sets are:
- $X \equiv -5 \equiv 1 \mod 3$
- $X \equiv -5 \equiv 4 = 1 + 3 \mod 9$
- $X \equiv -5 \equiv 22 = 1 + 3 + 2 \cdot 9 \mod 27$
- $X \equiv -5 \equiv 76 = 1 + 3 + 2 \cdot 9 + 2 \cdot 27 \mod 81$



This gives us the infinite 3-adic expansion:

- $X = -5 = 1 + 1\cdot 3 + 2\cdot 3^2 + 2\cdot 3^3 + \dots$

Note that we can't really _predict_ the digits in the 3-adic sequence of -5,
but we can keep expanding and finding more digits. 

Also see that the solutions are "coherent". In that, if we look at the
solution mod 9, which is $4$, and then consider it mod 3, we get $1$. So,
we can say that given a sequence of integers $0 \leq \alpha_n \leq p^n - 1$,
**$\alpha_n$ is p-adically coherent sequence** iff:

- $ \alpha_{n+1} = \alpha_n \mod p^n$.


#### Viewpoint: Solution sets of $X^2 = 25 \mod 3^n$

Since our solution sets are coherent, we can view the solutions as a tree,
with the expansions of $X = 5, X = -5 \mod 3$ and then continuing onwards
from there. That is, the sequences are

- $2 \rightarrow 5 \rightarrow 5 \rightarrow 5 \rightarrow \dots$
- $1 \rightarrow 4 \rightarrow 22 \rightarrow 76 \rightarrow \dots$

#### Solving $X^2 \equiv 2 \mod 7^n$

We now construct a solution to the equation $X^2 = 1$ in the 7-adic system,
thereby showing that $\mathbb Q_p$ is indeed strictly _larger_ than $\mathbb Q$,
since this equation does not have rational roots.

For $n=1$, we have the solutions as $X \equiv 3 \mod 7$, $X \equiv 4 \equiv -3 \mod 7$.

To find solutions for $n = 2$, we recall that we need our solutions to be consistent
with those for $n = 1$. So, we solve for:
- $(3 + 7k)^2 = 2 \mod 49$, $(4 + 7k)^2 = 2 \mod 49$.

Solving the first of these:

$$
\begin{align*}
(3 + 7k)^2 &\equiv 2 \mod 49 \\
9 + 42 k + 49k^2 &\equiv 2 \mod 49 \\
9 + 42 k + 0k^2 &\equiv 2 \mod 49 \\
7 + 42 k &\equiv 0 \mod 49 \\
1 + 6 k &\equiv 0 \mod 49 \\
k &\equiv 1 \mod 7
\end{align*}
$$

This gives the solution $X \equiv 10 \mod 49$. The other branch ($X = 4 + 7k$)
gives us $X \equiv 39 \equiv -10 \mod 49$.

We can continue this process indefinitely (_exercise_), giving us the sequences:

- $3 \rightarrow 10 \rightarrow 108 \rightarrow 2166 \rightarrow \dots$
- $4 \rightarrow 39 \rightarrow 235 \rightarrow 235 \rightarrow \dots$

We can show that the sequences of solutions we get satisfy the equation 
$X^2 = 2 \mod 7$. This is so by construction. Hence, $\mathbb Q_7$ contains
a solution that $\mathbb Q$ does not, and is therefore strictly bigger, since
we can already represent every rational in $\mathbb Q$ in $\mathbb Q_7$.


#### Use case: Solving $X = 1 + 3X$ as a recurrence

Let's use the tools we have built so far to solve the equation $X = 1 + 3X$.
Instead of solving it using algebra, we look at it as a recurrence $X_{n+1} = 1 + 3X_n$.
This gives us the terms:
- $X_0 = 1$
- $X_1 = 1 + 3$
- $X_2 = 1 + 3 + 3^2$
- $X_n = 1 + 3 + \dots + 3^n$

In $\mathbb R$, this is a divergent sequence. However, we know that the
solution so $1 + X + X^2 + \dots = 1/(1-X)$, at least as a generating function.
Plugging this in, we get that the answer should be:

- $1/(1 - 3) = -1/2$

which is indeed the correct answer.

Now this required some really shady stuff in $\mathbb R$. However, with a change
of viewpoint, we can explain what's going on. We can look at the above series
as being a series in $\mathbb Q_3$.  Now, this series does _really_ converge,
and by the same argument as above, it converges to $-1/2$.

The nice thing about this is that a dubious computation becomes a legal one
by changing one's perspective on where the above series lives.

#### Viewpoint: 'Evaluation' for p-adics

The last thing that we need to import from the theory of polynomials
is the ability to _evaluate_ them: Given a rational function $F(X) = P(X)/Q(X)$, 
where $P(X), Q(X)$ are polynomials, we can
evaluate it at some arbitrary point $x_0$, as long as $x_0$ is not a zero 
of the polynomial $Q(X)$.

We would like a similar function, such that for a fixed prime $p$, we obtain
a ring homomorphism from $\mathbb Q \rightarrow \mathbb F_p^x$, which we will
denote as $p(x_0)$, where we are imagining that we are "evaluating" the prime
$p$ against the rational $x_0$.

We define the value of $x_0 = a/b$ at the prime $p$ to be equal to
$ab^{-1} \mod p$, where $b b^{-1} \equiv 1 \mod p$. That is, we compute the
usual $ab^{-1}$ to evaluate $a/b$, except we do this $(\mod p)$, to stay with
the analogy.

Note that if $b \equiv 0 \mod p$, then we cannot evaluate
the rational $a/b$, and we say that $a/b$ has a pole at $p$. The order
of the pole is the number of times $p$ occurs in the prime factorization of $b$.

I'm not sure how profitable this viewpoint is, so I 
[asked on math.se](https://math.stackexchange.com/questions/3483369/profit-of-definition-evaluation-of-a-rational-at-a-p-adic),
and I'll update this post when I recieve a good answer.


#### Perspective: Forcing the formal sum to converge by imposing a new norm:

So far, we have dealt with infinite series in base $p$, which have terms
$p^i, i \geq 0$.
Clearly, these sums are divergent as per the usual topology on $\mathbb Q$.
However, we would enjoy assigning analytic meaning to these series. Hence, we
wish to consider a new notion of the absolute value of a number, which makes it
such that $p^i$ with large $i$ are considered small. 


We define the absolute value for a field $K$ as a function
$|\cdot |: K \rightarrow \mathbb R$. It obeys the axioms:


1. $\lvert x \rvert = 0 \iff x = 0$
2. $\lvert xy \rvert =  \lvert x \rvert  \lvert y \rvert$ for all $x, y \in K$
3. $\lvert x + y \rvert \leq \lvert x \rvert + \lvert y \rvert$, for all $x, y \in K$.


We want the triangle inequality so it's metric-like, and the norm to be
multiplicative so it measures the size of elements. 

The usual absolute value $\lvert x \rvert \equiv \\{ x : x \geq 0; -x : ~ \text{otherwise} \\}$ satisfies
these axioms.

Now, we create a new absolute value that measures primeness. We first introduce
a gadget known as a valuation, which measures the $p$-ness of a number. We use
this to create a norm that makes number smaller as their $p$-ness increases.
This will allow infinite series in $p^i$ to converge.

#### p-adic valuation: Definition

First, we introduce
a valuation $v_p: \mathbb Z - \\{0\\} \rightarrow \mathbb R$, where $v_p(n)$ is
the power of the prime $p^i$ in the prime factorization of $n$. More formally,
$v_p(n)$ is the unique number such that:

- $n = p^{v_p(n)} m$, where $p \nmid m$.
- We extend the valuation to the rationals by defining $v_p(a/b) = v_p(a) - v_p(b)$.
- We set $v_p(0) = +\infty$. The intuition is that $0$ can be divided by $p$
  an infinite number of times.

The valuation gets larger as we have larger powers of $p$ in the prime
factorization of a number. However, we want the norm to get _smaller_. Also,
we need the norm to be multiplicative, while $v_p(nm) = v_p(n) + v_p(m)$, which
is additive. 


To fix both of these, we create a norm by exponentiating $v_p$.
This converts the additive property into a multiplicative property. We
exponentiate with a negative sign so that higher values of $v_p$ lead to 
smaller values of the norm.

#### p-adic abosolute value: Definition

Now, we define the **p-adic absolute value** of a number $n$ as
$|n|_p \equiv p^{-v_p(n)}$.

- the norm of $0$ is $p^{-v_p(0)} = p^{-\infty} = 0$.
- If $p^{-v_p(n)} = 0$, then $-v_p(n) = \log_p 0 = -\infty$, and hence $n = 0$.
- The norm is multiplicative since $v_p$ is additive.
- Since $v_p(x + y) \geq \min (v_p(x), v_p(y)), |x + y|_p \leq max(|x|_p, |y|_p) \leq |x|_p + |y|_p$.
  Hence, the triangle inequality is also satisfied.

So $|n|_p$ is indeed a norm, which measures $p$-ness, and is smaller as $i$
gets larger in the power $p^i$ of the factorization of $n$, causing our
infinite series to converge.

There is a question of why we chose a base $p$ for $|n|_p = p^{v_p(n)}$. It would
appear that any choice of $|n|_p = c^{v_p(n)}, c > 1$ would be legal.
[I asked this on `math.se`](https://math.stackexchange.com/questions/3482489/why-does-the-p-adic-norm-use-base-p),
and the answer is that this choosing a base $p$ gives us the nice formula

$$
\forall x \in \mathbb Z, \prod_{\{p : p~\text{is prime}\} \cup \{ \infty \}} |x|_p = 1
$$

That is, the product of all $p$ norms and the usual norm 
(denoted by $\lvert x \rvert_\infty $ )
give us the number 1. The reason is that the $ \lvert x\rvert_p $ give us 
multiples $p^{-v_p(x)}$,
while the usual norm $\lvert x \rvert_\infty$ contains a multiple 
$p^{v_p(x)}$, thereby cancelling each other out.


#### Conclusion

What we've done in this whirlwind tour is to try and draw analogies between
the ring of polynomials $\mathbb C[X]$ and the ring $\mathbb Z$, by trying
to draw analogies between their prime ideals: $(X - \alpha)$ and $(p)$. So,
we imported the notions of generating functions, polynomial evaluation, and
completions (of $\mathbb Q$) to gain a picture of what $\mathbb Q_p$ is like.

We also tried out the theory we've built against some toy problems, that shows
us that this point of view maybe profitable. If you found this interesting,
I highly recommend the book 
[p-adic numbers by Fernando Gouvea](https://www.springer.com/gp/book/9783540629115).



# [Line of investigation to build physical intuition for semidirect products](#line-of-investigation-to-build-physical-intuition-for-semidirect-products)

To quote wikipedia:
> In crystallography, the space group of a crystal splits as the semidirect
> product of the point group and the translation group if and only if the space
> group is symmorphic

The if and only if is interesting: The geometry ofthe crystal lattice truly
appears to capture the structure of the semidirect product. It's a discrete
object as well, which makes it way easier to visualize. I'm going to hunt down
the definitions involved so I can finally feel like I truly understand semidirect
products from the "action" perspective.

# [Topology is really about computation --- part 2](#topology-is-really-about-computation--part-2)

Here, we're going to describe whatever I've picked up of sheaves in the past
couple of weeks. I'm trying to understand the relationship between sheaves,
topoi, geometry, and logic. I currently see how topoi allows us to model logic,
and how sheaves allow us to model geometry, but I see nothing about the
relationship! I'm hoping that writing this down will allow me to gain some
perspective on this.

## What is a sheaf?

Let's consider two sets $P, A$, $P \subseteq A$. Now, given a function
$f: A \rightarrow X$, we can restrict this function to $ A_P: P \rightarrow X $.
So, we get to _invert the direction_:

$$
(P \subseteq A) \iff (f: A \rightarrow X) \rightarrow (f_P: P \rightarrow X)
$$.

We should now try to discover some sort of structure to this "reversal"
business. Perhaps we will discover a contravariant functor! (Spoiler: we will).


# [Topology is really about computation --- part 1](#topology-is-really-about-computation--part-1)

Most people believe that topology is about some notion of "nearness" or
"closeness", which has been abstracted out from our usual notion of
continuity that we have from a metric space. Here, I make the claim that
topology is really about _computation_, and more specifically, _decidability_.
These are not new ideas. I learnt of this from a monograph [The topology of
computation](https://pdf.sciencedirectassets.com/272990/1-s2.0-S1571066104X00177/1-s2.0-S1571066104051357/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEJP%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJHMEUCIGQAb828p1io4csznEej60j0PwJteuXf7OoHLSCDhkUTAiEA9ITs1JrUEOE%2Ft%2Fl5TI9ZkNLUfBIx42IZ%2FoAqQpdX4twq2AII6%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FARACGgwwNTkwMDM1NDY4NjUiDBQOxJ3s3HCbVxSheCqsAt65yorZMMtIhLF7ML5sJQ9S5wZxBayKDrRkUKOjSzXxtQWebXs70FXhhpToXKvJoDrLgsqDzdF%2FAshZY%2FkDUep6KILxKnYxCBrBINhjFqxDlRZH0s1Y991RgCyNNnwmFI%2BrH70lSrV0OxQ9Z5WdXXPDkTLXv8512Xz%2BJn5DqEqdqD49FLbOuHl6PRM0TnYyNJkLBXNVwt75kkGkaTfdgmgiqh7YpcXcHlbqI2oqNcaxFDewXwKDCC7qWD6ECclLgszoeOXOtRI91nvNac8%2BLV4bXkKLXpd99H94N2vXPUPz99p6oqfdY9ixtcfI9POFX8agUilYjXKVhAWk4FSzzzMqbtZLBfkCZT4ffDTxRgL52yD%2FmL5E0Pe4mczVlUoB5DKoB8Lkitrt0BumQzDr4ffvBTrQAjVRuzG5V0CC%2Fd1t%2BUMPkrywaYytbrXCZ%2BkDo0xDBqsljY8DaGIiFINr8BEEpT7UX42GRhcDzpnOnztdAOTea3qZ3SmXJwgEoh0aiz%2B87MmsC57s0Q%2F%2B%2FDDvHBY3zLCrz7rdewXOgk6VxI9d5mhG3Du1dwPRbgOe798S2waDCD8LQA3rw7w5wNGa9Uv3xtNVH%2BHw%2FXcQ6OiubO4GL9mK8U5g7TVPh1hLB26XBQooKJ564VGf4J9VqWxjlx3NicVhqnFlGevNJNKyVLiyRsRCyQGMV59%2BXqUwdEMQZYWLbfUwELNz1NKfWumvu9BXC5jjsJgNx%2FRERSb7hqT1svMJU91o%2FHtatGAnPvVjYaNthha9O9jm%2BG9nw1vMsdyJ0asI5w5SrlsEyb5C7Vk7aLBcHAEi3XPRhivY1Q4hZAN0xY9VfEZrF%2FoM9HCGxr5cYs%2BP9w%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20191221T113658Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTY6QRXTPOC%2F20191221%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=8a26656e8f8c8772afdc2ae6caa9b583fd59a8ee6303d9770d25b4a3d8a6291f&hash=42518d3e5cc1e77ed961c359d0ea8f59bd582d1e5f13d69c3eeb2563a6c82abd&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S1571066104051357&tid=spdf-584e8188-4806-44bb-a118-9b63a53caaff&sid=f53691052392834d3e18a62484e2909639a9gxrqb&type=client), but this does not seem very well known,
so I decided to write about it.

The idea is this: We have turing machines which can compute things. We then
also have a set $S$. Now, a topology $\tau \subset 2^S$ precisely encodes
which of the subsets of $S$ can be separated from the rest of the space by a turing machine.
Thus, a discrete space is a very nice space, where every point can be separated
from every other point. An indescrete space is one where no point can be separated.

Something like the reals is somewhere in between, where we can separate 
stuff inside an open interval from stuff clearly outside, but there's some
funny behaviour that goes on at the boundary due to things like `(0.999... = 1)`,
which we'll see in detail in a moment.

#### Semidecidability

A subset $Q\subseteq S$ is _semidecidable_, if there exists a turing machine
$\hat Q: Q \rightarrow \{ \bot, \top \}$, such that:

$$
\begin{align*}
\hat Q(q) = \top \iff q \in Q \\
\hat Q(q) = \bot \iff q \notin Q \\
\end{align*}
$$

Where $\top$ signifies stopping at a state and returning \texttt{TRUE}, and
$\bot$ signifies \texttt{never halting at all}!. So, the subset $Q$ is
_semidedicable_, in that, we will halt and say \texttt{TRUE} if the element
belongs in the set. But if an element does not belong in the set, we are
supposed to never terminate.

#### Deep dive: semidecidability of the interval $(1, 2)$

Let's start with an example. We consider the interval $I = (1, 2)$, as a
subset of $\mathbb{R}$.Let the turing machine recieve the real number
as a function $f: \mathbb N \rightarrow \{0, 1, \dots 9\}$, such that
given a real number ${(a_0 \cdot a_1 \cdot a_2 \dots)}$, this is encoded as a
function ${f_a(i) = a_i}$. 

We now build a turing machine $\hat I$ which when given the input the function $f_a$,
semi-decides whether ${a \in I}$. 

Let's consider the numbers in $I$:

$$
0 \rightarrow \texttt{NO} \\
0.\overline{9} \rightarrow \texttt{NO} \\
1 \rightarrow \texttt{NO} \\
1.a_1 a_2 \dots \rightarrow \texttt{YES} \\
1.\overline{9} \rightarrow \texttt{NO} \\
2.0 \rightarrow \texttt{NO} \\
2.a_1 a_2 \rightarrow \texttt{NO}
$$

So, we can write a turing machine (ie, some code) that tries to decide whether
a real number $a$'s encoding $f_a$ belongs to the interval $I = (1, 2)$
as follows:

```py
def decide_number_in_open_1_2(f):
  # if the number is (1.abcd)
  if f(0) == 1:
    # (1.99...99x) | look for the x.
    # If the number is 1.999..., do not terminate.
    # if the number is any other number of the form 1.99..x, terminate
    i = 1
    while f(i) != 9: i += 1
    return
  # if the number is not 1.abcd, do not terminate
  while True: pass
```

Hence, we say that the interval $I = (1, 2)$ is _semi-decidable_, since we 
have a function
$\hat I \equiv \texttt{decide-number-in-open-1-2}$
such that
$\hat I (f_a) \text{ terminates } \iff a \in I$. 
We don't make _any claim_ about
what happens if $a \notin I$. This is the essence of semidecidability: We
can precisely state when elements in the set belong to the set, but not
when they don't.

### Semi decidability in general

To put this on more solid ground, we define a topology on a set $S$ by considering
programs which recieve as input elements of $S$, suitably encoded. For example,
the way in which we encoded real numbers as functions from the index to the
digit. Similarly, we encode other mathematical objects in some suitable way.

Now, we define:

- For every program $P$ which takes as inputs elements in $S$, the set 
  ${halts(P) \equiv \\{ s \in S \vert P(s) \text{halts} \\}}$ is called as a
  _semidecidable set_.

- Alternatively, we can say for a subset ${T \subset S}$, if there
  exists a program ${\hat T}$, such that
  ${s \in T \iff \hat T(s) \text{ halts}}$, then $T$ is semi-dedecidable.

These are just two viewpoints on the same object. In one, we define the
set based on the program. In the other, we define the program based on the
set.

### Semi decidability of the empty set and the universe set.

The empty set is semi-decidable, due to the existence of the program:
```py
def semidecide_empty(x):
  while True: continue
```


The universe set is semi-decidable, due to the existence of the program:
```py
def semidecide_univ(x): return
```

### Semi decidability of the union of sets

infinite unions of sets are semi decidable, since we can "diagonalize" on
the steps of all programs. That way, if any program halts, we will reach
the state where it halts in our diagonalized enumeration.



Let `A00, A01... A0n` be the initial states of the machines. We are trying to
semidecide whether any of them halt. We lay out the steps of the machines
in an imaginary grid:

```
A00 A01 A02 ... A0n
A10 A11 A12 ... A1n
A20 A21 A22 ... A2n
Am0 Am1 Am2 ... Amn
```

For example, machine `A0` has states:

```
A00 -> A10 -> .. -> Am0
```
We can walk through the combined state-space of the machines as:

```
A00
A01 A10
A02 A11 A20
A03 A12 A21 A30
...
```

Where on the `k`'th line, we collect all states $A_{ij}$ such that $(i + j = k)$.

Now, if any of the machines have a state that is `HALT`, we will reach the
state as we enumerate the diagonals, and the machine that explores the
combined state space can also return `HALT`.

### Semi decidability of the intersection of sets

infinite intersections of sets are _not_ semi decidable, since by running
these programs in parallel, we cannot know if an infinite number of programs
halt in finite time. We _can_ tell if _one_ of them halts, but of if _all_
of them halt.

For example, consider the sequence of machines produced by `machine_creator`:

```py
# creates a machine that stops after n steps
def machine_creator(n):
    # f terminates after n steps
    def f(x):
      for _ in range(n):
        continue
    return

    return f
```

We wish to check if the intersection of all `machine_creator(n)` halt, for all
$n \geq 0, n \in \mathbb N$. Clearly, the answer is an infinite number of steps,
even though every single machine created by `machine_creator` halts in a
finite number of steps.

# [PSLQ algorithm: finding integer relations between reals](#pslq-algorithm-finding-integer-relations-between-reals)

An algorithm to find _integer_ relations between _real_ numbers. It was
apparently named "algorithms of the century" by Computing in science and
engineering.

- [Wolfram link](http://mathworld.wolfram.com/PSLQAlgorithm.html)

# [Geometric characterization of normal subgroups](#geometric-characterization-of-normal-subgroups)
> $Stab(Orb(x)) = Stab(x) \iff Stab(x) \text{ is normal}$

> $\forall x' \in Orb(x), Stab(x') = Stab(x) \iff Stab(x) \text{ is normal}$


#### Forward: if the stabilizer is normal, then all elements in the orbit have the same stabilizer

Let a group $G$ act on a set $X$ with action $(~\dot~) : G \times X \rightarrow X$.  
Let $H \subseteq G$ be the stabilizer of a point $x \in X$. Now, let 
$K = kHk^{-1}$, a conjugacy class of $H$. Clearly, the element $(k \cdot x)$
in the orbit of $x$ is stabilized by $K$.

If the group $H$ is normal, then $K = H$. So every element in the orbit of $x$
is stabilized by $H$. 

#### Interaction of stablizer and the orbit:

> $Stab(g \cdot x) = g Stab(x) g^{-1}$

> $g^{-1} Stab(g \cdot x) g = Stab(x)$

-  Proof of $s \in Stab(x) \implies gsg^{-1} \in Stab(g \cdot x)$:
   The action of $gsg^{-1}$ on $g \cdot x$ is:
   $(g \cdot x \rightarrow_{g^-1} x \rightarrow_s x \rightarrow_g g \cdot x)$.

- Proof of $s' \in Stab(g \cdot x) \implies g^{-1}s'g \in Stab(x)$:
  The action of $g^{-1}s'g$ on $x$ is:
  $(x \rightarrow_{g} g \cdot x \rightarrow_{s'} g \cdot x \rightarrow_{g^{-1}} x)$.

Hence, both containments are proved.

#### Backward: if all elements in the orbit have the same orbit, then the stabilizer is normal.

From the above equation $Stab(g \cdot x) = g Stab(x) g^{-1}$. If the
entire orbit has the same stabilizer, $Stab (g \cdot x) = Stab(x)$. Hence,
we get $Stab(x) = g Stab(x) g^{-1}$, proving that it's normal.

# [Handy characterization of adding an element into an ideal, proof that maximal ideal is prime](#handy-characterization-of-adding-an-element-into-an-ideal-proof-that-maximal-ideal-is-prime)

##### The characterization

Let $I$ be an ideal. The ideal generated by adding $(a \in R)$ to $I$ is 
defined as $A \equiv (I \cup \{ a\})$. We prove that $A = I + aR$.


$$
(I \cup \{a \})  \\
= \quad \{ \alpha i + \beta a | i \in I, \alpha, \beta \in R \} \\
= \quad \{ i' + \beta a | i' \in I, \alpha, \beta \in R \} \qquad \text{($I$ is closed under multiplication by $R$)} 
= I + aR
$$

##### Quotient based proof that maximal ideal is prime 

An ideal $P$ is prime iff the quotient ring $R/P$ is an integral domain. An
ideal $M$ is maximal $R/M$ is a field. Every field is an integral domain,
hence:

$M \text{ is maximal } \implies R/M \text{ is a field } \implies R/M \text {is an integral domain} \implies M \text{ is prime}$.

I was dissatisfied with this proof, since it is not ideal theoretic: It argues
about the behaviour of the quotients. I then found this proof that argues
purly using ideals:

#### Ideal theoretic proof that maximal ideal is prime

##### Sketch
Let $I$ be a maximal ideal. Let $a, b \in R$ such that $ab \in I$. We need
to prove that $a \in I \lor b \in I$. If $a \in I$, the problem is done.
So, let $a \notin I$. Build ideal $A = (I \cup {a})$. $I \subsetneq A$. Since
$I$ is maximal, $A = R$. Hence, there are solutions for
$1_R \in A \implies 1_r \in I + aR \implies \exists i \in I, r \in R, 1_R = i  + ar$.
Now, $b = b \cdot 1_R = b(i + ar) = bi + (ba)r \in I + IR = I$. ($ba \in I$ by assumption).
Hence, $b \in I$.


##### Details

let $i$ be a maximal ideal. let $a, b \in r$ such that $ab \in i$. we need
to prove that $a \in i \lor b \in i$.

if $a \in i$, then the problem is done. so, let $a \notin i$. consider
the ideal $A$ generated by adding $a$ into $I$. $A \equiv (I \cup \{a\})$.

We have shown that $A = I + aR$. Hence, $I + a0 = I \subset A$. 
Also, $0 + ac \dot 1 = a \in A$, $a \neq I$ \implies $A \neq I$. Therefore,
$I \subsetneq A$. Since $I$ is maximal, this means that $A = R$

Therefore, $I + aR = R$. Hence, there exists some $i \in I, r \in R$ such
that $i + ar = 1_R$.

Now, $b = b \cdot 1_R = b \cdot (i + ar) = bi + (ba) r \in I + IR = I$ Hence,
$b \in I$.



# [Radical ideals, nilpotents, and reduced rings](#radical-ideals-nilpotents-and-reduced-rings)

##### Radical Ideals
A radical ideal of a ring $R$ is an ideal such that
$\forall r \in R, r^n \in I \implies r \in I$.
That is, if any power of $r$ is in $I$, then the element
$r$ also gets "sucked into" $I$.

##### Nilpotent elements
A nilpotent element of a ring $R$ is any element $r$ such that there exists
some power $n$ such that $r^n = 0$.

Note that every ideal of the ring contains $0$. Hence, if an ideal $I$
of a ring is known to be a radical ideal, then for any nilpotent $r$,
since $\exists n, r^n = 0 \in I$, since $I$ is radical, $r \in I$.

That is, _a radical ideal with always contain all nilpotents!_ It will
contain other elements as well, but it will contain nilpotents for sure.

##### Radicalization of an ideal
Given a ideal $I$, it's radical idea $\sqrt I \equiv \{ r \in R, r^n \in I \}$.
That is, we add all the elements $I$ needs to have for it to become a radical.

Notice that the radicalization of the zero ideal $I$ will precisely contain
all nilpotents. that is, $\sqrt{(0)} \equiv \{ r \in R, r^n = 0\}$.

##### Reduced rings
A ring $R$ is a reduced ring if the only nilpotent in the ring is $0$.

##### creating reduced rings (removing nilpotents) by quotienting radical ideals
Tto remove nilpotents of the ring $R$, we can create $R' \equiv R / \sqrt{(0}$. Since
$\sqrt{(0)}$ is the ideal which contains all nilpotents, the quotient ring $R'$ will contain
no nilpotents other than $0$.

Similarly, quotienting by any larger radical ideal $I$ will remove all nilpotents
(and then some), leaving a reduced ring.

> A ring modulo a radical ideal is reduced

##### Integral domains
a Ring $R$ is an integral domain if $ab = 0 \implies a = 0 \lor b = 0$. That is,
the ring $R$ has no zero divisors.

##### Prime ideals

An ideal $I$ of a ring $R$ is a prime ideal if
$\forall xy \in R, xy \in I \implies x \in I \lor y \in I$. This generalizes
the notion of a prime number diving a composite: $p | xy \implies p | x \lor p | y$.

##### creating integral domains by quotenting prime ideals

Recall that every ideal contains a $0$. Now, if an ideal $I$ is prime, and if
$ab = 0 \in I$, then either $a \in I$ or $b \in I$ (by the definition of prime).

We create $R' = R / I$. We denote $\overline{r} \in R'$ as the image of $r \in R$
in the quotient ring $R'$.

The intuition is that quotienting by a  $I$, since if $ab = 0 \implies a \in I \lor b \in I$,
we are "forcing" that in the quotient ring $R'$, if $\overline{a} \overline{b} = 0$, then either 
$\overline{a} = 0$ or $\overline{b} = 0$, since $(a \in I \implies \overline a = 0)$,
and $b \in I \implies \overline b = 0)$. 

> A ring modulo a prime ideal is an integral domain.


I learnt of this explanation from this
[excellent blog post by Stefano Ottolenghi](http://quickmathintuitions.org/relationship-between-reduced-rings-radical-ideals-and-nilpotent-elements/).

# [My disenchantment with abstract interpretation](#my-disenchantment-with-abstract-interpretation)

When I first ran across the theory of abstract interpretation, it seemed magical:
Define two functions, check that they're monotone maps, and boom, we have
on our hands an analysis.

However, the problem appears to be that in reality, it's not as simple. Here is
the list of issues I've run across when trying to use abstract interpretation
for a "real world" use-case:


First of all, all interesting lattices are infinte height, requiring some
choice of widening.  Defining a good widening is a black art.  Secondly, while
there is a lot of theory on combining abstract domains (reduced products and
the like), it seems hard to deploy the theory in the real world.

I read a fair bit into the theory of abstract acceleration, where the idea is
that instead of widening indiscriminately, if our theory is powerful enough to
compute an exact closed form, we choose to do so. However, the problem is that
this regime does not "fit well" into abstract interpretation: We have the
abstract interpreter on the one hand, and then the acceleration regime on the
other, which is a separate algorithm. So the full analysis looks something
like:

```python
def analyze(program):
  analysis = {}
  for loop in inner to outer:
     loop_data = abstract_interpret(loop)
     analaysis.append(accelerate(loop))
  return analysis
```

That is, what used to be a nice theory of just "do things in any order and
it will converge", now becomes a new algorithm, that uses abstract interpretation
as a subroutine. This was not the hope I had! I wanted to _get away_ from having
to do proofs by analyzing an algorithm, this was the entire promise: Define
a lattice well enough and the proof is guaranteed. Rather, what I had 
imagined was:

```python
def analyze(program):
  return abstract_interpret_using_acceleration_domain(program)
```

Now this `acceleration_domain` maybe frightfully complicated, but I'm willing
to pay that price, as long as it's an honest-to-god abstract interpretation.
This was a huge bummer for me to find out that this is not the case.


# [Computing equivalent gate sets using grobner bases](#computing-equivalent-gate-sets-using-grobner-bases)

Here's a fun little problem, whose only solution I know involves a fair
bit of math and computer algebra:

We are given the grammar for a language `L`:

```
E = T +_mod8 E | T -_mod8 E | T
T = V | V ^ V | V ^ V ^ V
V = 'a1' | 'a2' | ...
```


where `+_mod8` is addition modulo 8, `-_mod8` is subtraction modulo 8,
and `^` is XOR. 

This language is equipped with the obvious
evaluation rules, corresponding to those of arithmetic. We are guaranteed 
that during evaluation, the variables `a_i` will only have values `0` and `1`.
Since we have addition, we can perform multiplication by a constant
by repeated addition. So we can perform `3*a` as `a+a+a`.


We are then given the input expression `(a0 ^ a1 ^ a2 ^ a3)`. We wish
to find an equivalent expression in terms of the above language `L`.

We think of `E` as some set of logic gates we are allowed to use, and we are
trying to express the operation `(a0 ^ a1 ^ a2 ^ a3)` in terms of these gates.

The first idea that I thought was that of employing a grobner basis,
since they essentially embody rewrite rules modulo polynomial equalities, which
is precisely our setting here.

In this blog post, I'm going to describe what a grobner basis is and why it's
natural to reach for them to solve this problem, the code, and the eventual
solution.

As a spolier, the solution is:

```
a^b^c^d =
-a - b + c + 3*d - 3*axorb - axorc
+ axord - bxorc + bxord + 3*cxord 
- 3*axorbxorc - axorbxord 
+ axorcxord + bxorcxord
```

Clearly, this contains only additions/subtractions and multiplications by
a constant.

If there's some principled way to derive this (beyond throwing symbolic
algebra machinery), I'd really love to know --- 
[Please raise an issue with the explanation!](https://github.com/bollu/bollu.github.io/issues)

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
S = AB + B^2 -1-> 1B + B^2 = B + B^2 (STUCK)
```

However, if we apply `(2)` and then `(1)`:

```
S = AB + B^2 -2-> -B^2 + B^2 -> 0
```

This tells us that we _can't just apply the rewrite rules willy-nilly_. 
It's sensitive to the _order_ of the rewrites! That is, the rewrite system
is not [confluent](https://en.wikipedia.org/wiki/Confluence_(abstract_rewriting)).


The grobner basis is a function from rewrite systems to rewrite systems.
When given a rewrite system `R`, it produces a _new_ rewrite system `R'`
that _is confluent_. So, we can apply the rewrite rules of `R'` in any order,
and we guaranteed that _we will only get a 0 from `R'` if and only if
we could have gotten a `0` from `R`_ for all strings.

We can then go on to phrase this whole rewriting setup in the language of
ideals from ring theory, and that is the language in which it is most 
often described. [I've gone into more depth on that perspective here: "What is a grobner basis? polynomial
factorization as rewrite systems"](#what-the-hell-is-a-grobner-basis-ideals-as-rewrite-systems).

Now that we have a handle on what a grobner basis is, let's go on to solve
the original problem:


##### An explanation through a slightly simpler problem

I'll first demonstrate the idea of how to solve the original problem
by solving a slightly simpler problem:

> Rewrite `a^b^c` in terms of `a^b`, `b^c`, `c^a` and the same `+_mod8` instruction
> set as the original problem. The only difference this time
> is that we do _not_ have `T -> V ^ V ^ V`.


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


# xor of 2 numbers as a polynomial
def xor2(x, y): return x + y - 2*x*y

# xor of 3 numbers as a polynomial
def xor3(x, y, z): return xor2(x, xor2(y, z))

# define the ideal which contains relations:
# xor2(a, b) -> axorb, xor2(b, c) -> bxorc, xor2(a, c) -> axorc
# we also add the relation (a^2 - a = 0 => a = 0 or a = 1)
# since we know that our variables are only {0, 1}
I = ideal((axorb - xor2(a, b), bxorc - xor2(b, c), axorc - xor2(a, c), a*a-a, b*b-b, c*c-c))

# the polynomial representing a^b^c we wish to reduce
f_orig = xor3(a, b, c)

# we take the groebner basis of the ring to reduce the polynomial f.
IG = I.groebner_basis()

# we reduce a^b^c with respect to the groebner basis.
f_reduced = f_orig.reduce(IG)

print("value of a^b^c:\n\t%s\n\treduced: %s" % (f_orig, f_reduced))

# Code to evaluate the function `f` on all inputs to check correctness
def evalxor2(f):
    for (i, j, k) in [(i, j, k) for i in [0, 1] for j in [0, 1] for k in [0, 1]]:
      ref = i^^j^^k
      eval = f.substitute(a=i, b=j, c=k, axorb=i^^j, bxorc=j^^k, axorc=i^^k)
      print("%s^%s^%s: ref(%s) =?= f(%s): %s" % 
        (i, j, k, ref, eval, ref == eval))

# check original formulation is correct
print("evaulating original f for sanity check:")
evalxor2(f_orig)

# Check reduced formulation is correct
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


# [The janus programming language --- Time reversible computation](#the-janus-programming-language--time-reversible-computation)

- [Wiki link](https://en.wikipedia.org/wiki/Janus_(time-reversible_computing_programming_language))
- [Original letter to Landlauer](http://tetsuo.jp/ref/janus.pdf)

I found out it's called Janus, since Janus is the god of doorways in greek
mythology. Hence, he is also the god of duality and transitions --- he
_literally_ looks both into the future and into the past.

> He is usually depicted as having two faces, since he looks to the future and
> to the past.

An apt name for the language!

# `A = B` --- A book about proofs of combinatorial closed forms


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
   a b c d e
1| 0 0 1 1 1 (LSB)
```

We start with all bits at their lowest position. Now, we try to go to
the next smallest number, which still has 3 bits toggled. Clearly, we need
the bit at position `b` to be 1, since that's the next number. Then,
we can keep the lower 2 bits `d, e` set to 1, so that it's still as small a
number as possible.

```
   a b c d e
2| 0 1 0 1 1 (LSB)
```

Once again, we now move the digit at `d` to the digit at `c`, while keeping
the final digit at `e` to make sure it's still the smallest possible.

```
   a b c d e
3| 0 1 1 0 1 (LSB)
```

Now, we can move the `1` at `e` to `d`, since that will lead to the smallest
increase:

```
   a b c d e
4| 0 1 1 1 0 (LSB)
```

At this point, we are forced to move to location `a`, since we have exhausted
all smaller locations. so we move the `1` at `b` to `a`, and then reset all
the other bits to be as close to the LSB as possible:

```
   a b c d e
5| 1 0 0 1 1 (LSB)
```

Continuing this process gives us the rest of the sequence:

```
    a b c d e
5 | 1 0 0 1 1
6 | 1 0 1 0 1
7 | 1 0 1 1 0
8 | 1 1 0 0 1 (note the reset of d!)
9 | 1 1 0 1 0
10| 1 1 1 0 0
```
# [Bondi k-calculus](#bondi-k-calculus)

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

# [Vivado toolchain craziness ](#vivado-toolchain-craziness)

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
/hashing.elf: error while loading shared libraries:
libxilinxopencl.so: cannot open shared object file: No such file or directory
```
At this point, clearly we have some linker issues (why does `xocc` not correctly statically link? What's up with it? Why does it expect it to be able to load a shared library? **WTF is happening**). do note that this is _not_ the way the process
is supposed to go according to the tutorial!  
- Of course, there's no static library version of `libxilinxopencl.so`, so that's a dead end. I'm completely unsure if the tutorial even makes sense. 
- This entire chain of debugging is full of luck.

- [Link talking about generating `BOOT` file](https://www.xilinx.com/html_docs/xilinx2018_2/sdsoc_doc/compiling-and-running-applications-on-arm-processor-hjy1504034381720.html)


At some point, I gave up on the entire enterprise.

# [What the hell _is_ a Grobner basis? Ideals as rewrite systems](#what-the-hell-is-a-grobner-basis-ideals-as-rewrite-systems)

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

How can we apply the rewrite rules $xy \rightarrow -1$, $y^2 \rightarrow 1$,
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


# [Lie bracket versus torsion](lie-bracket-versus-torsion)


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


# [Spatial partitioning data structures in molecular dynamics](#spatial-partitioning-data-structures-in-molecular-dynamics)

- [Cell lists](https://en.wikipedia.org/wiki/Cell_lists)
- [Verlet lists](https://en.wikipedia.org/wiki/Verlet_list)

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

# [Everything you know about word2vec is wrong](#everything-you-know-about-word2vec-is-wrong)

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

# [Small Haskell MCMC implementation](#small-haskell-MCMC-implementation)

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
# [Debugging debug info in GHC](#debugging-debug-info-in-GHC)


I wanted to use debug info to help build a better debugging experience
within [`tweag/asterius`](http://github.com/tweag/asterius). So, I was 
reading through the sources of `cmm/Debug.hs`.  
I'd never considered how to debug debug-info, and I found the information
tucked inside a cute note in GHC (`Note [Debugging DWARF unwinding info]`):

> This makes GDB produce a trace of its internal workings. Having gone this far,
> it's just a tiny step to run GDB in GDB. Make sure you install debugging
> symbols for gdb if you obtain it through a package manager.

- [Link to GHC sources](https://github.com/ghc/ghc/blob/535a26c90f458801aeb1e941a3f541200d171e8f/compiler/cmm/Debug.hs#L458)


# GHC LLVM code generator: Switch to unreachable

The [switch to out of range](https://github.com/ghc/ghc/blob/master/compiler/llvmGen/LlvmCodeGen/CodeGen.hs#L1102) 
code generator switches to the first label. It should be more profitable
to switch to a `unreachable` block. That way, LLVM can take advantage of UB.

# Concurrency in Haskell:

Great link to the GHC wiki that describes the concurrency primitives
"bottom up": https://gitlab.haskell.org/ghc/ghc/wikis/lightweight-concurrency

# [Handy list of differential geometry definitions](#handy-list-of-differential-geometry-definitions)

There are way too many objects in diffgeo, all of them subtly connected.
Here I catalogue all of the ones I have run across:

##### Manifold

A manifold $M$ of dimension $n$ is a topological space. So, there is a
topological structure $T$ on $M$. There is also an _Atlas_, which is a family
of _Chart_s that satisfy some properties.

##### Chart

A chart is a pair $(O \in  T , cm: O -> \mathbb R^n$. The $O$ is an open set of the
manifold, and $cm$ ("chart for "m") is a continuous mapping from $O$ to $\mathbb R^n$
under the subspace topology for $U$ and the standard topology for $\mathbb R^n$.

#####  Atlas

An _Atlas_ is a collection of _Chart_s such that the charts cover the manifold,
and the charts are pairwise compatible. That is, $A = \{ (U_i, \phi_i) \}$, such
that $\cup{i} U_i = M$, and $\phi_j \circ phi_i^{-1}$ is smooth.

##### Differentiable map

$f: M \to N$ be a mapping from an $m$ dimensional manifold to an $n$ dimensional
manifold. Let $frep = cn \circ f \circ cm^{-1}: \mathbb R^m -> \mathbb R^n$ 
where $cm: M \to \mathbb R^m$ is a chart for $M$, $cn: N \to \mathbb R^n$ 
is a chart for $N$. $frep$ is $f$ represented
in local coordinates. If $frep$ is smooth for all choices of $cm, cn$,
then $f$ is a differentiable map from $M$ to $N$.

##### Curve: 

Let $I$ be an open interval of $\mathbb R$ which includes the point `0`.  A Curve is a
differentiable map $C: (a, b) \to M$ where $a < 0 < b$.

##### Function: (I hate this term, I prefer something like Valuation): 

A differentiable mapping from $M$ to $R$.


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




# [Lazy programs have space leaks, Strict programs have time leaks

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

# [Presburger arithmetic can represent the Collatz Conjecture](#presburger-arithmetic-can-represent-the-collatz-conjecture)

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


# Japanese Financial Counting system

- [Wikipedia](https://en.wikipedia.org/wiki/Japanese_numerals#Formal_numbers)

Japanese contains a separate kanji set called `daiji`, to prevent people
from adding strokes to stuff previously written.

```
#  |Common |Formal
1  |一     |壱
2  |二     |弐
3  |三     |参
```


# Stephen wolfram's live stream

- [Twitch.tv link](https://www.twitch.tv/videos/408653972)


I've taken to watching the live stream when I have some downtime and want
some interesting content. 

The discussions of Wolfram with his group are great, and they bring up
_really_ interesting ideas (like that of cleave being very irregular).

# `Cleave` as a word has some of the most irregular inflections
- cleave
- clove
- cleaved
- clave
- cleft

# McCune's single axiom for group theory

[Single Axioms for Groups and Abelian Groups with Various
Operations](http://ftp.mcs.anl.gov/pub/tech_reports/reports/P270.pdf)
provides a single axiom for groups. This can be useful for some ideas I have
for training groups, where we can use this axiom as the loss function!

# `Word2Vec` C code implements gradient descent really weirdly
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


# Arthur Whitney: dense code
- Guy who wrote a bunch of APL dialects, write code in an eclectic style
  that has very little whitespace and single letter variable names.
- Believes that this allows him to hold the entire program in his head.
- Seems legit from my limited experience with APL, haskell one-liners.
- [The b programming language](http://kparc.com/b/readme.txt). It's quite 
  awesome to read the sources. For example, [`a.c`](http://kparc.com/b/a.c)

# How does one work with arrays in a linear language?

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

# How Linear optimisation is the same as Linear feasibility checking
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

# Quantum computation without complex numbers
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


# Linguistic fun fact: Comparative Illusion

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
