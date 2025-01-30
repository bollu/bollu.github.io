#import "@preview/ctheorems:1.1.3": *
#show: thmrules

By default, all $k = overline(k)$ and $text("char")(k) = 0$.

== Affine $n$ space
Affine $n$ space over $k$ is the set $A^n_k equiv k^n$.

A polynomial $f(x_1, dots.h, x_n) in k[x_1, dots.h, x_n]$ is a function on affine space $A^n \to A$.
The zero set of vanishing locus of a set of polynomials $S subset k[x_1, dots.h, x_n]$ is
the collection of points where all the polynomials vanish: $\{ p in A^n : f(p) = 0 forall f in S \} equiv Z(S)$.
An _affine algebraic set_ is any subset of some $A^n$ of the form $Z(S)$.

=== Examples of affine algebraic sets

+ $A^n = Z(0)$.
+ $emptyset = Z(1)$.
+ in $A^2$, $x y$ is the union of $x$ and $y$ axes, but in $A^3$, $Z(x y z)$ is the union of planes. (union of codimension 1 surfaces).
  The axes in $A^3$ are $Z(x y, y z, x z)$. (at least two coordinates must vanish).

== Hypersurface
If $f in k[x_1, dots.h, x_n]$ is a non-constant polynomial, then $Z(f)$ is called a hypersurface.
In the particular case that $f$ is linear, this is a hyperplane.

=== More Examples of affine algebraic sets

(Key example upon projectivization). 
$\{ (t, t^2, t^3) in A^3 : t in k \}$ is the twisted cubic in $A^3$.
We claim it's an affine algebraic set, equal to $Z(y - x^2, z - x^3)$.
The twisted cubic is _non-planar_, and is not contained in any hyperplane.


=== Properties of Zero Sets of Ideals
Recall from groups, rings, modules that $k[x_1, dots.h, x_n]$ is noetherian, and it has unique factorization.

==== Proposition
Let $S subset.eq k[x_1, dots.h, x_n]$. (1) Then let $I equiv (S)$. Then $Z(I) = Z(S)$.
(2) $exists f_1, dots.h, f_r in S$ such that $Z(S) = Z(\{f_1, dots.h, f_r\})$. i.e. only finitely many polynomials needed to write down any algebraic set.

==== Proof
For part (1), clearly $S subset.eq I$, so $Z(I) subset.eq Z(S)$, since if all elements of $I$ vanish, then all elements of $S$ vanish.
On the other hand, elements of $I$ are linear combinations of elements of $S$. Thus, if $p in Z(S)$, then every $f in S$ vanishes, and thus every
$sum_i alpha_i f_i$ also vanishes.

For part (2), since the ring is noetherian, we know that $I$ is finitely generated. Let generators be $\{ f_i \}$. Thus, $Z(I) = Z(\{f_i\})$.

==== Note
The association $I arrow.r.bar Z(I)$ is order-reversing.

==== Proposition
Affine algebraic sets satisfy:
(1) $S subset.eq T subset.eq k[x_1, dots.h, x_n]$, then $Z(T) subset.eq Z(S)$.
(2) $A^n, emptyset$ are affine algebraic sets.
(3) If $\{S_i\}_{i in I$ is a collection of subsets of $k[x_1, dots.h, x_n]$, then $sect_i Z(S_i) = union_i S_i$.
(4) $Z(S) union Z(T) = Z(S times T)$.

=== Distinguished Opens
A distinguished open set in $A^n$ is any set given by the nonvanishing of a single polynomial, so ${ x in A^n : f(x) eq.not 0 }$, or $A^n - Z(f)$.


=== Products
Although $A^m times A^n = A^(m + n)$ as sets, the product of the zariski topologies is not the zariski topology of the product.

=== Subspaces
If $X subset.eq A^n$ is an affine algebraic set, it inherits the subspace topoogy.
