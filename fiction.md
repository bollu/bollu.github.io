
# Moduli Space
- Samples from the moduli space of all possible mathematics.

- [Analytic dependent type thoery](#analytic-dependent-type-theory)
- [Cohomology of haskell types](#cohomology-of-haskell-types)
- [Finite differences of haskell types](#finite-differences-of-haskell-types)
- [Derivatives of function spaces](#derivatives-of-function-spaces)
- [Segment trees from Mobius inversion](#segment-trees-from-mobius-inversion)
- [Billiard ball computation](#billiard-ball-computation)
- [On the geometry of distributed algorithms](#on-the-geometry-of-distributed-algorithms)
- [Compilers as commutative algebra](#compilers-as-commutative-algebra)


# [Analytic dependent type thoery](#analytic-dependent-type-theory)

Till the early half of the 21st century, we were building type theories
built with algebraic substrates. This made them excellent for reasoning
about `A = B`, but lousy for reasoning with `forall epsilon > 0, |A - B| < epsilon`.
The breakthrough came with three crucial, intertwined insights:

- We should build logics of _relations_, not _functions_.

- To compute with relations, we should use _prolog_-like languages, where
  computations are non-deterministic, and whose computational content
  is about relations.

- Relations power order theory, which powers topology. Once topology comes
  online, we have the world. Computation follows from denotation. 


This new breed of languages begin with a theory built of first order logic 
and [_order theory_](https://en.wikipedia.org/wiki/Order_theory), contrasting the
[Calculus of constructions](https://en.wikipedia.org/wiki/Calculus_of_constructions),
which begins with first-order logic with _equality_. One quickly builds
up topology from this, either ala [Scott](https://en.wikipedia.org/wiki/Scott_continuity),
or via [Locales](https://en.wikipedia.org/wiki/Pointless_topology). 

Working topologically, we forgo the notion of equality for the notion of
limits. We never state that `x = y`. Rather, we state that for any sequence of
radii `epsilon_i`, we will always have that `|x - y| < epsilon_i`, where the
partial order `<` is a primitive of our logic.  This is of course, horrid for
algebra. The upshot is that we can do analysis.


We discover that programming language semantics, which is really the study of
computability, which is really topology, works out beautifully in this
setting, as first discovered by [Scott and Stratchey](https://en.wikipedia.org/wiki/Denotational_semantics), later
vigorously expanded under the cumbersome title of [synthetic topology of data types and classical spaces](https://www.sciencedirect.com/science/article/pii/S1571066104051357). Cumbersome proofs
of program properties are the exception, rather than the norm.


# [Cohomology of haskell types](#cohomology-of-haskell-types)

We know that some types such as $3x^2$ can be integrated to fill
a hole, giving us $x^3$. On the other hand, $x^2$ cannot be integrated:
it gives $x^3/3$. Is there some kind of cohomology that governs this?
Note that right now we don't have $d^2 = 0$ --- This is something we will
need to find.


This sort of thinking might help if we are trying to identify how to lay
data down in memory; having zero homology would imply that there's no gaps,
so we can lay it out contiguously in-memory.


# Finite differences of haskell types

- What happens if we take finite differences? We know that real derivatives
  give us one-hole contexts.

$$
\begin{align*}
&fdiff(list x) \\
&= fdiff(1/[1-x])  \\
&= 1/[1-(x+h)] - 1/[1-x]  \\
&= ([1-x] - [1-(x+h)])/[1-x][1 - (x+h)]  \\
&= ([1-x] - [1-x-h])/[1-x][1 - (x+h)]  \\
&= (1 - x - 1 + x + h)/[1-x][1 - (x + h)]   \\
&= h/[1-x][1 - (x + h)]   \\
&= (h, list x, list (either x h))
\end{align*}
$$

$$
\begin{align*}
&bdiff(list x) \\
&= fdiff(1/[1-x])  \\
&= 1/[1-x] - 1/[1-(x - h)]  \\
&= ([1-x] - [1-(x-h)])/[1-x][1-(x-h)] \\
&= ([1-x] - [1-x+h])/[1-x][1-(x-h)] \\
&= (1-x - 1 +x-h))/[1-x][1-(x-h)] \\
&= -h/[1-x][1-(x-h)] \\
\end{align*}
$$

- https://en.wikipedia.org/wiki/Divided_differences

# [Derivatives of function spaces](#derivatives-of-function-spaces)

we can write $\texttt{list}(x)$ as $f: \texttt{nat} \rightarrow x \simeq  x^{\texttt{nat}}$.
If we attempt to compute the derivative, we get:

$$
\begin{align*}
&\frac{d}{dx} (x^{\texttt{nat}}) \\
&= \textttt{nat} x^{\texttt{nat} - 1}
\end{align*}
$$

which is saying "tell me the index of the missing element" --- that is `nat`,
and also keep a list with a hole: that is, $x^{\texttt{nat} - 1}$.

# Segment trees from Mobius inversion

- Mobius inversion allows us to collect information about $g(\leq x)$ in a
  function $f$

- Segment trees / fenwick trees also let us do this. They let us collect
  information about lists.

- If we have an algebra of indexees `Ix List` and an algebra of 
  'segments of a list' `Seg List`, then the fenwick tree is a way to go
  from repr. to the other. Refer to the orbit representation.

- What happens if we replace `List` with `Tree`? `Ix` is easy. What about
  `Seg`? Can we have the euler tour 'pop out' from this? How?


This is quite easy. We can use `Traversable` to fill any data structure;
we can use the APL technique of `fold /= 0` to check for beginning-and-ending
of matching parity.


# Snooker on a Doughnut: A promenade of geometric computation theory.

#### Paper abstract

We consider the hamiltonian evolution of systems on 
symplectic manifolds. We perform classical 
computations on these manifolds using the billiard ball
model of computation. We investigate the relationship
of time complexity, space complexity (volume of phase space),
and gate complexity (billiard ball count)
as a function of the manifold. We also exhibit 
concrete examples of where this model returns results which are
exponentially better than the best known lower bounds
for space and gate complexity. Intuitively, the power
of this model comes from allowing rearrangements of the billiard
ball evolution. The balls act as inputs, outputs, and
gates. As a result, a curious form of self-modifying
programming (which allows access to better complexity classes) is formed.
We propose three new lines of research: uncovering the
complexity theoretic phenomena that is present, studying the symplectic geometry 
that is controlling the situation, and the intruguing 
possibility of building such a machine.


#### Reflections

A quest for truth, flashes of brilliance, a suicide: the tangled
history of geometric computation theory has it all. Here, we narrate the
story, not for the layperson as has been done by many excellent
expositions, but for the advanced undergraduate who wishes to learn
the mathematical morphogenisis of modern geometric computation theory.

It was a well known fact that billiard balls could be
used to model classical computation. However, this avenue was never deeply
explored, due to the existence of "universal computability" results among
the theoretical computer science community.

In parallel, There were curious artefacts discovered by those studying
molecular modelling --- they had recently received incredible speedups
by quantizing their models using finite-element-style methods, only amplified
by the use of discrete differential geometry. Discrete morse theory made it
possible to study the critical points of extremely complicated functions,
which created a new wave of software (and programming languages) decicated to
expressing discrete geometry. A grad student, performing incorrect floating
point initialization found that his computations terminated far too quickly.
More intruguingly, the rendering of the final electron potential appeared to
look like the spacetime of the evolution of the heat equation in 1D.

Excited, a short monograph was published in Physical Review A. In the span
of a month, a new model of computation was rapidly being uncovered. It turned
out that the manifold that had been expressed from the incorrect initialization
was in fact the n-fold torus. This led to the electrons bouncing and re-arranging,
leading to the ability to freely mix inputs, computation, and outputs. For example,
balls that had been used to form gates would fly apart on receiving input,
spiral around the hypertorus, and then reconvene at precisely the right
moment to create _new gates_. This way, self-organising computation could be
arranged on compact manifolds, with far greater performance (sometimes asymptotically)
than the best-known classical algorithms. [The compactness is a technical condition
required for conservation of billiard balls in a finite volume of spacetime].


New complexity classes such as $BILLIARD(n, M, \omega, T)$ were invented --- 
the class of decision problems solvable using $n$ billiard balls on a compact,
symplectic manifold $(M, \omega)$ using $T(\cdot)$ time.

Geometric complexity theory, which had languished after the Pfaffians was
revitalized. Three quantities were discovered to be intimately related:

1. The perimeter of the manifold, which parametrized the space of possible
   inputs --- by Aaronson's theorem, we can always be setup the inputs of a 
   billiard ball computation to lie on the boundary of the manifold.
2. The length of the longest period of time that could be spent before
   the system reached back to its initial state, which was a measure of
   time.
3. The volume of the manifold, which is a measure of space.

The trinity of space-time-inputs were mutually bounded using generalizations of the
isoperimetric inequality which were adapted to this setting. This led to
the resolution of the `logspace` versus `ptime` hierarchy, but complicated
the messy waters of circuit complexity with _new_ classes of circuits.

In the meantime, experimentallists gradually learnt how to  completely control
spacetime locally, to reshape it to any compact manifold we desire. This led
to an explosion of interest in realising billiard ball models of computation,
since under certain hypotheses, it was possible to _calculate_ initial billiard
ball positions for very complicated calculations --- the evolution of the solution
of the schrodinger wave equation for the helium atom in 3D required only 100
billiard balls self-organising on the algebraic variety generated
by the Vakil equations (recall that algebraic varieties in $\mathbb C^n$ 
are Kahler manifolds, and are hence Symplectic). 

What was very unclear was whether we could _setup_ these initial positions
as necessary. Initial complexity theoretic considerations were unsettling. 
Finding optimal configurations was proven to be `EXPSPACE`-complete. However,
heuristic algorithms run fast enough that many conjecture that understanding
the class of "good" heuristics may provide breakthroughs into
average-case-complexity: the gap between what is really possible and what the
theory predicts is far too wide.

Intel, desperate to make the "law" their founder had setup made an unprecedented
pivot into billiard ball computing. They rebranded it into the fashionable
"symplectic computing units (SPUs)", which were capable of delivering massive
parallelism and far better clock speeds than their CPU counterparts. Funnily
enough, this also sunk the EDA industry. All their IP-protected coveted tools
for place-and-route were made obsolete by the new, open-source tools by Intel
that could place and route symplectic computations. Someone found a cute hack
to redo classical place-and-route in terms of symplectic optimisation. Just like
that, a billion dollar industry came crashing down.

In the end, I suppose the complexity theorists are the saddest of the lot:
they're saddled with many new conjectures, but haven't
resolved any of the old ones.

#### Exercises 

Prove that analogue of $P \neq NP$ in the symplectic regime by using:

- $NP$ corresponds to trajectories for many particles, each along a leaf of a foliation of the manifold.
- The foliated space has a larger cicardin rhythm $\Lambda$, thereby allowing
  for longer-time orbits than the base space.
- Design a manifold whose foliation allows for an asymptotic difference
  in $\Lambda$, thereby proving $P \neq NP$ 


# On the geometry of distributed algorithms

##### Abstract

We begin with what a model of
computation for distributed systems: processes on a graph
which send values from semi-lattices; state variables updated with
a monotonic join; conditionals are upward
filters. This implicitly ensures parallelism, idempotence,
and strong eventual consistency. We then recast this
setting into a more computational regime, where
states are vectors in $\mathbb Z^n$, join is
pointwise `max`, and 
computations are piecewise-linear functions.
We then proceed to move to tropical geometry, to associate 
a tropical varietry to the above distributed algorithm. This
allows us re-interpret all previously known ideas of 
distributed algorithms in a purely geometric setting.
From this pivot, we compute cohomology groups, from which 
we derive clasically known impossibilities --- 
the PAC theorem, byzantine one-in-third-impossibility,
as statements about the non-triviality of the first and higher
cohomology groups of the geometry. Finally, present many previously
known communication complexity invariants as being
topological invariants of the scheme.


# Compilers as commutative algebra.

# Complexity theory as monad composition.

# Coinduction as coarse space theory.


