# Moduli Space
- Samples from the moduli space of all possible mathematics.

- [Billiard ball computation](billiard-ball-computation)
- [Compilers as commutative algebra](compilers-as-commutative-algebra)

# Billiard ball computation

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
arranged on compact manifolds [The compactness is a technical condition
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

I suppose the lesson is that floating point operations can cause the stock
market to crash, just not in the way you expect it to.

#### Exercises 

Prove that analogue of $P \neq NP$ in the symplectic regime by using:

- $NP$ corresponds to trajectories for many particles, each along a leaf of a foliation of the manifold.
- The foliated space has a larger cicardin rhythm $\Lambda$, thereby allowing
  for longer-time orbits than the base space.
- Design a manifold whose foliation allows for an asymptotic difference
  in $\Lambda$, thereby proving $P \neq NP$ 


# Compilers as commutative algebra

# Complexity theory as monad composition

# Coinduction as coarse space theory

# Distributed systems as lattice cohomology.

A lull is present in the distributed systems community, as they continually
construct more and more contrived models of 
