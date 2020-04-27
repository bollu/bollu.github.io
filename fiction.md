# Moduli Space
- Samples from the moduli space of all possible mathematics.

- [Billiard ball computation](billiard-ball-computation)
- [On the geometry of distributed algorithms](#on-the-geometry-of-distributed-algorithms)
- [Compilers as commutative algebra](compilers-as-commutative-algebra)

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

# Snooker on a Doughnut: A promenade of geometric computation theory.

#### Paper abstract

We consider the hamiltonian evolution of systems on 
symplectic manifolds. We perform classical 
computation on these manifolds with the billiard ball
model of computation. We investigate the relationship
of time complexity, space complexity (volume of phase space),
and gate complexity (billiard ball count)
as a function of the manifold. We exhibit 
concrete examples where this model is
exponentially  than the best known lower bounds
for space and gate complexity. Intuitively, the power
of the model is to allow rearrangements of the billiard
ball evolution to have the balls act as inputs, outputs, and
gates. This permits a curious form of self-modifying
programming that allows access to better complexity classes.
We propose three new lines of research: one on uncovering the
complexity theoretic phenomena that is present, another
on the symplectic geometry that is controlling the situation,
and finally, the intruguing possibility of building such a machine.


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


