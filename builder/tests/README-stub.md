<!-- comment out katex, it doesn't work as well as mathjax :(
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.css" integrity="sha384-zB1R0rpPzHqg7Kpt0Aljp8JPLqbXI3bhnPWROx27a9N0Ll6ZP/+DiW/UqRcLbRjq" crossorigin="anonymous">
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.js" integrity="sha384-y23I5Q6l+B6vatafAwxRu/0oK/79VlbSz7Q9aiSZUvyWYIYsd+qj+o24G5ZU2zJz" crossorigin="anonymous"></script>
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>
<script>
document.addEventListener("DOMContentLoaded", function() {
  console.log("invoked katex...");
  renderMathInElement(document.body, { delimiters : [
  {left: "$$", right: "$$", display: true},
  {left: "!!", right: "!!", display: true},
  {left: "$", right: "$", display: false},
  {left: "\\[", right: "\\]", display: true},
  {left: "%[", right: "%]", display: true},
  ] });
  console.log("finished running katex");
});
</script>
-->



<!-- comment out mathjax
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
> </script>
<script
  type="text/javascript"
  charset="utf-8"
  src="https://vincenttam.github.io/javascripts/MathJaxLocal.js"
> </script>
-->



Suppose we have a manifold $M$. of dimension $d$ that has been embedded isometrically
into $\mathbb R^n$. So we have a function $e: \mathbb R^d \rightarrow \mathbb R^n$
which is the embedding. We will identify $M$ to be the subspace $Im(e)$.

Recall that $\partial_{x_i} e : \mathbb R^d \rightarrow \mathbb R^n$
is defined as:

$$
\begin{align*}
&\partial_{x_i}e : \mathbb R^d \rightarrow \mathbb R^n \\
&[\partial {x_i}e](p) \equiv
 \lim_{\delta x \rightarrow 0} \frac{e(p + (0:0, 1:0\dots, i:\delta_x, \dots, n:0)) - e(p)}{\delta x}
\end{align*}
$$

Note that it is a function of type $\mathbb R^d \rightarrow \mathbb R^n$.


- The tangent space at point $p \in Image(e)$ is going to be spanned by
  the basis $\{ \partial_{x_i}e \vert_p : \mathbb R^n \}$.


- The metric tensor of $M$,
 $g_{ij} \equiv \langle \frac{\partial e}{\partial x_i} \vert \frac{\partial e}{\partial x_j} \rangle$.
 That is, the metric tensor "agrees" with the dot product of the
 ambient space $\mathbb R^n$.

- A vector field $V$ on the manifold $M$ is by definition a combination of
  the tangent vector fields. $V(p_0) \equiv v^j(p_0) \partial_{x_j} e(p_0)$


We can calculate the derivaive of this vector field as follows:

$$
\begin{align*}
&\frac{V(p)}{\partial x^i} \\
&= \partial_{x_i} \left[ v_j(p) \partial_{x_j} e \right]
&= v^j \cdot \partial_{x_i} \partial_{x_j} e + \partial_{x_j}e \cdot \partial_{x_i} v^j
\end{align*}
$$

We choose to rewrite the second degree term in terms of the tangent
space, and some component that is normal to us that we have no
control over.

$$
(\partial_{x_i} \partial_{x_j} e )(p) \equiv \Gamma^k_{ij} \partial_{x_k} e + \vec{n}
$$

This gives us the Christoffel symbols as "variation of second derivative _along_
the manifold.



$$
\begin{align*}
\nabla_{e_i} V &\equiv \partial_{x_i} V - \vec{n}  \\
 &= \Pi_{\vec{n}^\bot} \left [v^j \cdot \partial_{x_i} \partial_{x_j} e + \partial_{x_j}e \cdot \partial_{x_i} v^j \right]
 &= \Pi_{\vec{n}^\bot} \left[ v^j \cdot (\Gamma^k_{ij} \partial_{x_k} e + \vec{n})+ \partial_{x_j}e \cdot \partial_{x_i} v^j \right]
 &= v^j \cdot (\Gamma^k_{ij} \partial_{x_k} e + \vec 0) + \partial_{x_j}e \cdot \partial_{x_i} v^j
 &= v^j \cdot (\Gamma^k_{ij} \partial_{x_k} e + \vec 0) + \partial_{x_k}e \cdot \partial_{x_i} v^k
 &= v^j \cdot \Gamma^k_{ij} \partial_{x_k} e  + \partial_{x_k}e \cdot \partial_{x_i} v^k
 &= \partial_{x_k} e \left( v^j \cdot \Gamma^k_{ij}  + \partial_{x_i} v^k \right)
\end{align*}
$$




On learning about infinite dimensional vector spaces, one learns that
we need to use the axiom of choice to assert that every such vector space
has a basis; indeed, it's equivalent to the AoC to assert this. However,
I had not known any "natural" examples of such a vector space till I studied
the proof of the barvinok algorithm. I produce the example here.

Consider a space such as $S \equiv \mathbb R^3$. Now, consider the vector
space spanned by the indicator functions of polyhedra in $S$. that's a mouthful,
so let's break it down.

A polyhedra is defined as a set of points that is defined by linear
inequalities: $P \equiv \{ x \in S : a_i \cdot x \leq b_i, i \in [1\dots n] \}$,
for all $a_i \in S$, $b \in \mathbb R$.

The indicator functions are of the form:

$$
[poly]: S \rightarrow \mathbb R;
[poly](x) \equiv
\begin{cases} 1 & x \in poly \\
0 & \text{otherwise} \end{cases}
$$

we can define a vector space of these functions over $\mathbb R$, using
the "scaling" action as the action of $\mathbb R$ on these functions:

$$
\begin{align*}
 &(f + g)(x) \equiv f(x) + g(x) \\
 &(r \cdot f)(x) \equiv r \times f(x)
\end{align*}
$$

The vector space $V$ is __defined__ as the span of the indicator functions
of all polyhedra. It's clearly a vector space, and a hopefully intuitive
one. However, note that the set we generated this from (indicators of polyhedra)
don't form a basis since they have many linear dependencies between them.
For example, one can write the equation:

![indicator-polyhedra-relations](./static/indicator-polyhedra-relations.png)


# [Krohn-Rhodes decomposition](#krohn-rhodes-decomposition)

We denote partial functions with $X \rightharpoonup Y$ and total functions
with $X \rightarrow Y$.

A set $X$ equipped with a binary operator
$\star: X \times X \rightarrow X$ which is closed and associative
is a semigroup.

#### Partial function semigroup

For a ground set $X$, the set of partial functions $Pf(X) \equiv \{ f: X \rightharpoonup X \}$
along with function composition forms a semigroup. This is in fact stronger
than a semigroup. There exists:

- An identify function $e_x: X \rightarrow X; e_X(x) = x$
- A zero function $\theta_x: X \rightharpoonup X; \theta_x(x) = undef$, where by
  $undef$ we mean that it is _undefined_.

#### Transformation semigroup(TS)

Let $Q$ be a set. Let $S \subseteq Pf(Q)$ be a sub-semigroup of $Pf(Q)$.
Then the semigroup $X \equiv (Q, S)$ is called as the
 _transformation semigroup_(X) of states $Q$.

- The elements of $Q$ are called states of $X$
- while  the elements of $S$ are called  _actions_ of $X$.
- The set $Q$ itself is called as the _underlying set_ of $X$.
- For a fixed transformation semigroup $X$, we will write $Q_X$ and $S_X$
  to refer to its states and actions.


We call $X \equiv (Q, S)$ as a _transformation monoid_ if $S$ contains $1_Q(q) = q$.

##### Subtlety of being at transformation monoid.

There is some subttlety here. Just because $S$ is a monoid does not mean that
it that is a _transformation monoid_. It must have the identity element of
$Pf(Q)$ to be called a transformation monoid. For example, consider the
set $Q \equiv \\{ a, b \\}$ and the transformation semigroup $S \equiv \\{ f \equiv \alpha \mapsto b \\}$.
Now the set $S$ is indeed a monoid with identity element as $f: Q \rightarrow Q$.
however, $f \neq 1_Q$ , andh ence, $S$ is a not a _transformation monoid_.



##### Examples of transformation semigroups

1. $(X, \{ \theta(x) = undef \})$. The semigroup with the empty transformation.

2. $(X, \emptyset)$, the semigroup with _no_ transformations.


#### Semigroup action

We sometimes wish to represent a semigroup using an action/transformation semigroup
on a ground set $X$. So, given some semigroup $(T, \times)$ that needs to be represented,
if we can find a morphism $r: T \rightarrow Pf(X)$ ($r$ for representation)
such that:

- $ r(t_1 + t_2) = r(t_1) \circ r(t_2)$. [$r$ is a semigroup morphism].
- $t_1 \neq t_2 \implies \exists x \in X$ such that $r(t_1)(x) \neq r(t_2)(x)$.
  [Faithfulness].

Put more simply, $t_1 \neq t_2 \implies r(t_1) \neq r(t_2)$ where we define
function equality extensionally: $f = g \equiv \forall x, f(x) = g(x)$.


#### Embedding actions into the transformation semigroup
We often wish to represent some semigroup $S$ as the transformation semigroup
of some set of states $Q$. We can achieve this by proving a morphism:

- $r: S \rightarrow Pf(Q)$ that is faithful.

Then, we can treat elements of $S$ as elements of $Pf(Q)$.


#### Completion of a transformation semigroup

Given a transformation semigroup $X \equiv (Q, S)$ we can _complete_ it
by adding a new sink state $\bot$, and then converting all partial
functions in $S$ to total functions that transition to $\bot$. We have that
$\bot \cdot s = s \cdot \bot ~ \forall s \in S$.

We denote the completion as $X^c \equiv (Q^c, S^c)$.



#### Coverings

Let $X \equiv (Q_X, S_X)$ and $Y \equiv (Q_Y, S_Y)$ be transformation
semigroups. Let $\phi \subseteq Q_Y \times Q_X$ be a relation. Let $s_x \in S_X$
and $s_y \in S_Y$. Then, if the following diagram commutes:

$$
\begin{array}{ccc}
a & \rightarrow & b
\downarrow & & \downarrow
x & \rightarrow & y
\end{array}
$$

If $s_x(\phi(q_y)) = \phi(t_y(q_y))$, then we say that $t_y$ covers $s_x$ relative to $\phi$.
We imagine the $t_y$ lying above $s_x$, being projected down by $\phi$.

If a fixed $\phi$, for all $s_x \in S_X$ there exists a $t_y \in S_Y$ such that
$t$ covers $s$ relative to $\phi$, then we say that $\phi:$ is a _relation of
automata_.

- If $\phi: Q_Y \rightarrow Q_X$ is surjective,
  then we say that $\phi$ is a __relational covering__ and write:

$$
X \triangleleft_{\phi} Y
$$

- If $\phi \subeteq Q_Y \times Q_X $ is _both_ surjective and partial,
  then we say that $\phi$ is a __covering__ and write:

$$
X \prec_\phi Y
$$

If $X \prec_\phi Y$, we say that $Y$ dominates $X$, or $Y$ covers $X$, or
$X$ divides $Y$.

#### Checking coverings and generating subsets

We note that for a given covering $\phi$, if $s_x$ is covered by $t_y$
and $p_x$ is covered by $q_y$, then $s_x \circ t_x$ is covered by $t_y \circ q_y$.

Thus, to check if $X$ is covered by $Y$, we simply need to check if 
__some generating subset of $X$ is covered by $Y$__.

#### Checking coverings of representations

Let us assume we have a representation of a transformation semigroup
given with a semigroup $\Sigma$, a transformation semigroup
$X \equiv (Q_X, S_X)$, and a representation $r: \Sigma \rightarrow S_X$ that is
faithful.

Now, to check that $X$ is covered by another $Y$, it suffices to check that
there exists a $t_y \in Y$ for each $\sigma \in X$ such that $r(\sigma)$ is
covered by this $t_y$.

#### Companion relation
Given a relation $\phi: Y \rightarrow X$, then we define:

$$
\Sigma \equiv \{ (s, t) : \text{$t \in T_Y$ covers $s \in S_X$ \}
$$

Recall compositions of elements are covered by a composition
of their coverings. Hence, if $(s, t), (s', t') \in \Sigma$, then
$(ss', tt') \in \Sigma$. thus, $\Sigma$ is a subsemigroup of $S_X \times S_Y$.

We can regard $\Sigma$ as the graph of a relation $\phi' \subseteq Q_Y \times Q_X$.
This will be called as __companion relation__ of $\phi$.

#### Wreath products

Let $X \equiv (Q_X, S_X)$ and $Y \equiv (Q_Y, S_Y)$. We're going to define a large
product $X \wr Y$.

We begn with the set $W \equiv S_X^Q_Y \times S_Y$, where
$S_X^Q_Y \equiv \{ f : Q_Y \rightarrow S_X \}$.
The wreath product then becomes:

$$
X \wr Y \equiv (Q_X \times Q_Y, W)
$$

with the action of $W$ on an element of $Q_X \times Q_Y$ being defined as:

$$
(f : Q_Y \rightarrow S_X, s_y : S_Y) (q_x : Q_X, q_Y : Q_Y) \equiv ( f(q_y)(q_x) , s_y (q_y))
$$

it's a "follow the types" sort of definition, where we edit the right component
as $r_y \mapsto t_y(r_y)$ since that's all we can do. In the case of
the left component, we have a $q_x$, and we need to produce another element
in $Q_X$, so we "must use $f$". The only way to use $f$ is to feed it
a $t_y$. This forces us into the above definition.


##### Composition of wreath products

To show that its closed under composition, let's consider $(f, s_y), (g, t_y) \in W$
with $f, g: Q_Yg \rightarrow S_X$, and $s_y, t_y \in S_Y$. The result is
going to be:

$$
(f, s_y)  (g, t_y) =  (\lambda q_y. f(q_y) \circ g(q_y), t_y \circ u_y)
$$

#### Equivalences of subsets of states

Let $X = (Q, S)$ be a transition system. Given subsets $(a, b, \subseteq Q)$,
we shall write $b \leq a$ if either $b \subseteq a$ or there exists some $s \in S$
such that $b \subseteq sa$, where $s(a) \equiv \{ s(a_i) : a_i \in a\}$. We can
define an equivalence relation $a \sim b \iff a \leq b \land b \leq a$.

**Note**: $ b \leq a \implies |b| \leq |a|$, since 
$b \leq a$ means that $b \subseteq s(a)$. Note that $s$ is actually a
function $s: Q \rightarrow Q$, and a function mapped over a set can only
ever decrease the number of elements in a set, since a function can only
xglomp elements together; it can never break an element apart into two.
Hence, $b \subseteq sa \subseteq a$, and thus $|b| \leq |a|$.


Similiarly, $a \leq b \implies |a| \leq |b|$. Therefore, $b \sim a$ means
that $|b| = |a|$.

**Theorem**: for all $a, b \in Q_X$ such that
$a ~ b$ such that $b \subseteq s(a)$, we show that $b = s(a)$, and there exists
a $t \in S_X$ such that $a = t(b)$.

**Proof**: Since $b \subseteq s(a) \subseteq a$ and $|b| = |a|$, $b = s(a)$.
Therefore $s$ is a permutation. Hence, $s$ is invertible and there exists
an inverse permutation $t$ such that $a = t(b)$. We now need to show that
$t \in S_X$. To do this, first note that if the order of the permutation
$s$ is $n$, then $t = s^{n-1}$, since $t \circ s = s^{n-1} \circ s = 1_S$.
Since the semigroup $S$ is closed under composition $t = s^{n-1}$ is in $S$,
since it is $s$ composed with itself  $(n-1)$ times.

#### Subset families of interest

We will be interest in a family of subsets of $Q_X$ called $A$, of the form:
- all sets of the form $s(Q)$ for all $s \in S_X$
- the set $Q$
- the empty set $\emptyset$
- all the singleton sets $\{ q \}$ for all $q \in Q$.

In the above set, we have $\leq$ and $\sim$ as defined above.

We note that the set $A$ is **closed under the action of all $s \in S_X$**.
For example, the empty set is taken to the empty set. All singleton
sets are taken to other singleton sets. For the full set $Q$, we add
the sets $s(Q)$ for all $s \in S_X$.

#### Height function

A height function for a transition system $X \equiv (Q_X, S_X)$ is a function
$h: A \rightarrow \mathbb Z$ such that:


1. $h(\emptyset) = -1$.
2. $h(\{ q \}) = 0 \forall q \in Q$.
3. $a \sim b \implies h(a) = h(b)$ for all $a, b \in A$.
4. $b < a \implies h(b) < h(a)$ for all $a, b \in A$.

The notation $b < a \equiv (b \leq a) \land \lnot (a \leq b)$.

(3) + (4) imply that two elements of the same height are either equivalent
or incomparable.

#### Pavings and bricks

for $a \in A$ such that $|a| > 1$, we denote by $B_a$ the set of all $b \in A$
what are maximal subsets of $a$. That is, if $b \in B_a$ then $b \subsetneq a$,
and $\not \exists c, b \subsetneq c \subsetneq a$. Equivalently, if there
exists a $c$ such that $b \subseteq c \subseteq a$, then $b = c$ or $b = a$.

Note that we can assert that $a = \cup_{b \in B_a} b$. This is because $B_a$
contains all the singletons of $Q_X$. so we can begin by writing $a$ as
a union of singletons, and then merging elements of $B_a$ into larger elements
of $B$, terminating when we cannot merge any more elements of $B_a$.

- The set $B_a$ is called as the **paving of $a$**.
- The elements of $B_a$ are called as the **bricks of $a$**.

#### Group of permutations for $a \in A$

Let us assume that there exists a $s \in S$ such that $s(a) = a$. Let $A_a$
be the set of all elements in $A$ contained in $a$:
$A_a = \{ A_i : A_i \in A, A_i \subseteq a \}$.

Recall that the set $A$ was closed under the action of all $s$, and hence,
since $s$ is a permutation of $a$, this naturally extends into a
permutation of $A_a$: $s A_a = A_a$. Now note that this induces a permutation
of the set $B_a$. This creates a transition system:

\begin{align*}
&G_a \equiv \{ s \in S : s a = a} \\
&H_a \equiv (B_a, G_a) \\
\end{align*}

We have already shown how if $s \in S$ defines a permutation of some set $X$
by its action, then its inverse also exists in $S$. So, this means that
$G_a$ is in fact a transition _group_ that acts on $B_a$.

It might turn out that $G_a = \emptyset$. However, if $G_a \neq \emptyset$,
then as stated above, $G_a$ is a group.

We will call such a transition group a **generalized transition group**, since
either $G_a = \emptyset$ or $G_a$ is a group.


Now, the generalized transition group $H_a$ is called as the
**holonomy transition system** of $a$, and the group $G_a$ is called as
the **holonomy group** of $a$.


We have that $G_a \prec S$ since $G_a$ is a quotient of the sub-semigroup
$\{ s | s \in S, as = a \}$. (TODO: so what? why does this mean that it's $\prec$?)

**Theorem:** if $a \sim b$, then $H_a \simeq H_b$
  (similar subsets have isomorphic holonomy transition systems).
**Proof:** Let us assume that $a \neq b$. since $a \sim b$, we have elements
of the form $s, s^{-1} \in S$ such that $b = s(a)$, $a = s^{-1}(b)$.

Recall that for $b_a \in B_a$ is such that for a member $g \in G_a$,
$g(b_a) = b_a$. $B_b$ must have the element $s(b_a)$. [TODO!]

#### Holonomy decomposition

Let $X \equiv (Q, S)$ be a transition system and let $h$ be a height
function for $X$, such that $h(Q) > 0$. For a fixed $i$,
let $a_1, a_2, \dots a_k$ be the representatives of equivalence classes of
elements of $A$ of height equal to $i$. We define:

$$
H_i^\lor \equiv H_{a_1} \lor H_{a_2} \dots \lor H_{a_n}
$$


#### Inductive hypothesis for coverings

We will say a relational covering $X \triangleleft_{\phi} Y$ is **of rank $i$**
with respect to a given height function $h$ if $\phi$ relates states in $Y$
to subsets of states in $x$ that are members of $A$ and have rank at most i.
Formally, for each $p \in Q_Y$, we have that $\phi(p) \in A$ and$h(\phi(p)) \leq i$.


We prove that if $X \triangleleft_{\phi} Y$ is a relational covering of rank $i$,
then $X \triangleleft_{\phi} \overbar{H_i^\lor} \wr Y$ is a relational covering
of rank $i - 1$.

The proof is a proof by induction.

##### Base case:

Start with the relational covering with $Q_Y = \{ 0 \}, S_Y = \{ id \}$,
and the cover $\phi(0) = Q_X$. Clearly, this has rank $n$ since the height
of $Q_X$ is $n$, and $\phi$ is inded a covering, since the only transition
that $Y$ can make (stay at the same state) is simulated by any transition
in $S_X$ [TODO: is this really the argument?]

For induction, assume $X \triangleleft_{\phi} Y$ is a relational covering of rank $i$
with respect to some height function $h$. $X\equiv (Q_X, S_X)$ and
$Y \equiv (Q_Y, S_Y)$. We define
- $QY_i \equiv \{ q_y : q_y \in Q_Y, h(\phi(q_y)) = i \}$
- $QY_< \equiv \{ q_y : q_y \in Q_Y, h(\phi(q_y)) < i \}$


We know that $A$ contains elements of height exactly $i$. Let $a_1, a_2, \dots a_k$
be representatives of sets of of height $i$ in $A$. Thus, for each $qy_i \in QY_i$,
we have that:

- $\phi(qy_i) = a_j$ for a **unique** $1 \leq j \leq k$.
- We select elements $u, \overline{u} \in S$ such that $u(\phi(qy_i)) = a_j$
  and $\overline{u}(a_j) = \phi(qy_i)$.

We will show how to establish a relational covering:
- $X \triangleleft_{\phi} \wr \overbar{H_i^\lor} Y$ using a relation:
- $\phi \subseteq [(B_{a_1} \cup B_{a_2} \cup \dots B_{a_k})\times Q_Y ] \times Q_X$

#### References

- Automata, Languages and Computation by Elinberg.
- [On the Krohn-Rhodes decomposition theorem by Oded Maler](http://www-verimag.imag.fr/~maler/Papers/kr-new.pdf)
- [Ideas of the Holonomy Decomposition of Finite Transformation Semigroups](http://www.egri-nagy.hu/pdf/holonomy_general.pdf)
- [Nine chapters on the semigroup art](http://www-groups.mcs.st-andrews.ac.uk/~alanc/pub/c_semigroups/c_semigroups_a4.pdf)
- [Computational holonomy decompositions of transformation semigroups](http://www.biomicsproject.eu/file-repository/category/CompHolonomy.pdf)

# [Proving block matmul using program analysis](#proving-block-matmul-using-program-analysis)

It's a somewhat well-known fact that given matrix multiplication: $O = AB$
where $O \in \mathbb R^{2n \times 2m}$ ($O$ for output),
$A \in \mathbb R^{2n \times r}, B \in \mathbb R^{r \times 2m}$ are matrices.

