Finite Abelian grouos and their Charactera
The Chapter starts with Postulates of a group.

- A group is a three tuple $\langle G,., e \rangle$, where $G$ is a non-empty set of elements and $.$ is the group operator, $e$ is the identity.
Postulates of a Group 
- Closure
- Associativity
- Existence of Identity
- Inverse. 

An Abelian Group is a commutative group.

$\forall a,b \in G : ab=ba$

Finite Groups : If G is a finite set.
Subgroup : A non empty Subset of $G$, $G'$, which itself is a group with the same group operator.

#### Thm 6.1:
If Elements $a,b,c \in G$ satisfy:

 $$ ac=bc \lor ca=cb $$
then $a=b$.

If $G'$ is a subgroup of $G$, then for any element $a \in G$
we call $n \in \mathbb $ to be an Indicator of $a$ if $a^n \in G'$,
and $n$ is the smallest positive integer $[\neq 0]$.

#### Thm 6.6

Let $G'$ be a subgroup of a finite abelian group $G$, where $G \neq G'$. 
Choose an element $a \in G / G'$ and let $h$ be it's indicator.
Then the set of products:

$$ G'' = \{ xa^k : x \in G' \land k = 0,1,2\dots,h-1 \} $$
is a subgroup of G which contains G'. Order of G'' is h times that of G'.


#### Charaters of Finite abelian groups

##### Definition of Character:
Let $G$ be an arbitrary finite group. A complex valued function $f: G \rightarrow \mathbb C^\times$ defined on $G$ is called a character if $f$ is a 
group homomorphism. That is, it has the multiplicative property:
$$ f(ab) = f(a)f(b) $$

##### Theorem  6.7

If f defined over FAG $G$ which has indentity $e$, then $f(e)=1$
and each function value f(a) is a root of unity.
$$
a^n =e \quad f(a)^n=1
$$

Hence, to be more accurate, we can write $f: G \rightarrow U(1)$ where
$U(1) \equiv \{ c \in C : |c| = 1 \}$


#### Theorem  6.8
A FAG of order $n$ has exactly $n$ distinct characters.

$G'$ is a proper subgroup of $G$..
$G''$ is constructed from $G'$ using 
- $G_1=\{ e \}$
- $G_2 = \langle G_1,a_1 \rangle$
- $G_3 = \langle G_2,a_2 \rangle$
- $G_{r+1} = \langle G_r,a_r \rangle$

$$ G_1 \subsetneq G_2 \subsetneq  G_3 \dots \subsetneq G_{t+1}  = G$$

- $G_1$ cleary has 1 distinct character: $f(e) = 1$.
- assume for induction that $G_r$ has $r$ distinct characters.
- Let  $h$ be the indicator of $a_r$ in $G_{r+1} = \langle G_r, a_r \rangle$

- From $G_r$ to $G_r+1$ there are exactly $h$ different ways to extend each character of $G_r$

- We'll have $mh$ characters of $G_{r+1}$ which is the same as it's order.

##### How do we extend?

- $f$ is a character on $G_{r+1}$, 
- any element in $G_{r+1}$ is of the form $x.a_r^k$ 
- Let's we extend some character $f$ to $f'$.
- $f'(x \cdot a_r^k)= f'(x)f'(a_r)^k$.
- since $x \in G_r,  f'(x) = f(x)$.
- How many choices do we have for $f'(a_r)$?
- Let $h$ be the indicator of $a_r \in G_{r+1}$ relative to $G_r$.
  That is, $a_r \not \in G_r$, but $a_r^h \in G_r$.
  
If we have an element $c =a_r^h$
c \in G_r,
$f'(c) = f(c)$

$f'(a_r)^h = f(c)$
$f'(a_r)^h = 1 \cdot f(c)$
$f'(a_r) = e^{2\pi k/h} \cdot f(c)^{1/h}$


So now for the value of $f'(a_r)$ we have h choices.
So we can extend f into h new f'.

TODO: show that this is a legit group hom. [I believe this]
f'(xa_r^k.ya_r^j) = f'(xa_r^k)f'(ya_r^j)

#### The Charater Group

This section $G$ is a FAG of order n.
The principle/principal character is called $f_1$, is the function $f_1(\_) = 1$. The others, denoted by $f_2,f_3, \dots f_n$ ar called non-principal charactes. They have the property that $f(a)\neq 1$ for some $a \in G$.


##### Thm 6.9

If multiplication of characters is defined by the relation:
- $(f_i \cdot f_j)(a) \equiv f_i(a)f_j(a)$ for each $a \in G$

then set of characters forms an Abelian group of order $n$. The group is dentoed by $\hat G$. The identity is $f_1$. The inverse is of $f_i$ is the reciprocal $1/f_i$.

**Note:** For each $f$ we have $|f(a)| = 1$, since it is the character of a FAG.
- $1/f(a)$ becomes congugate of f(a).
- $\overline{f}(a) \equiv \overline{f(a)}$ is also a character of G.
$$
\overline{f}(a) = \frac{1}{f(a)} = f(a^{-1})
$$

Does there exist a morphism $\phi: G \rightarrow \hat G$?

#### Orthogonality relations for characters

$G$ = FAG of order n with elements $a_1, a_2, ... a_n$
and let $f_1, f_2,...,f_n$ be the characters of G, with $f_1$ being principal character.

- We denote $A = A(G)$ the $n\times n$ matrix $[a_{ij}]$ whose element $a_{ij}$ in the $i$th row and $j$th column is 
$$
a_{ij} \equiv f_{i}(a_{j})
$$

##### Thm 6.10
The sum of entries in the $i$th row of $A$ is given by
$$
\sum_{r=1}^n  f_{i}(a_r) \equiv \begin{cases}
n & \text{if  $f_i$ is the principal character ($i=1$)} \\
0 & \text{otherwise}
\end{cases}
$$

- For f_i = f_1 every value is 1 so sum is n.

- For $f_i \neq f_1$, there is an element $b \in G$ such that $f(b) \neq 1$ as $a_r$ runs through elements of G so does product $b \cdot a_r$, hence:


$$
S =\sum_{r=1}^n  f_{i}(ba_r) = f_i(b)\sum_{r=1}^n  f_{i}(a_r) = f_i(b)S
$$

- $S(1-f_i(b)) =0$
- $f_i(b) \neq 1$ so $S=0$ [slick how $f_i(b) \neq 1$ shows up.]


##### Thm 6.11
Let $A^\star$ be the transpose conjugate of $A$. Then we have
- $AA^\star = nI$,
where $I$ is $n\times n$ identity matrix. Hence $n^{-1} A^\star$ is the Inverse of $A$.

Let $B = AA^\star$
The entry $b_{ij}$ in the ith row and jth column of B is
$$
b_{ij} \equiv 
\sum_{r=1}^{n} f_i(a_r)\overline{f_j}(a_r) 
= \sum_{r=1}^n (f_i\overline{f_j})(a_r) 
= \sum_{r=1}^n f_k(a_r)
$$
Where $f_k = f_i\overline(f_j) = f_i/f_j$. Now $f_i/f_j = f_1$ if and only if $i=j$, Hence by thm 6.10 we have:
$$
b_{ij} = n \delta_{ij}
$$
$B = nI$

- TODO: this reminds me of a fourier transform; what's the relation?

#### Thm 6.12  ---  Orhtogonatilty Relations for Characters
We have:

$$
\sum_{r=1}^{n} \overline{f_r}(a_i)f_r(a_j) = n \delta{a_i,a_j}
$$

##### Proof:

$$
AA^\star = nI \\
C=A^{\star}A=nI. \\
$$
Then the left side of the sum is $c_{ij}$.
Now since $\overline{f_r}(a_i) = f_r(a_i)^{-1} = f_r(a_i^{-1})$

$$
\sum_{r=1}^n f_r(a_i^{-1}a_j) = n\delta{a_i,a_j}
$$


If $a_i = e$
then we have Thm 6.13:
The sum of entries of jth column of A is given by:
$$\sum_{r=1}^n f_r(a_j) = n\delta{aj,e}$$


