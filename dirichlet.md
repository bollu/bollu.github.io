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
we call $n \in \mathbb N$ to be an Indicator of $a$ if $a^n \in G'$,
and $n$ is the smallest positive integer $[\neq 0]$.

#### Thm 6.6

Let $G'$ be a subgroup of a finite abelian group $G$, where $G \neq G'$. 
Choose an element $a \in G / G'$ and let $h$ be it's indicator.
Then the set of products:

$$ G'' = \{ xa^k : x \in G' \land k = 0,1,2\dots,h-1 \} $$
is a subgroup of $G$ which contains $G'$. Order of $G''$ is $h$ times that of $G'$.


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
$f'(xa_r^k.ya_r^j) = f'(xa_r^k)f'(ya_r^j)$ [Can be proved]

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
Where $f_k = f_i\overline{(f_j)} = f_i/f_j$. Now $f_i/f_j = f_1$ if and only if $i=j$, Hence by thm 6.10 we have:
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
then we have 
#### Thm 6.13:
The sum of entries of jth column of A is given by:
$$\sum_{r=1}^n f_r(a_j) = n\delta{aj,e}$$



16/05/2020

#### Dirichlet Characters

From here $G$ is group of reduced residue classes modulo a fixed positive integer $k$.

First we prove that $G$ is a group if a multiplication is suitably defined.

#### Reduced Residue system modulo $k$
A set of $\varphi(k)$ integers $\{a_{1},a_{2},\dots,a_{\varphi(k)}\}$ incongruent modulo $k$, each of which is relatively prime to $k$.

- $a_i \not \equiv a_j \mod k \quad \forall i \neq j$
- $\gcd(a_i, k) = 1 \quad \forall i$

For each integer $a$ the corresponding residue class $\hat{a}$ is the set of all integers congruent to $a$ modulo $k$:
$$
\hat{a}=\{x:x\equiv a \mod k\}
$$

We define multiplication of residue classes by the relation:
$$
\hat{a}\ . \hat{b} = \hat{ab}
$$

#### Thm 6.14

With multiplication defined as above, the set of reduced residue classes modulo k is finite abelian group of order $\varphi(k)$. Isn't this group  $\mathbb Z/k \mathbb Z^\times$?
- Identity : $\hat{1}$
- Inverse of $\hat{a}$ : $\hat{b}$ such that $ab \equiv1\ mod\ k$.


### Definition of Dirichlet Characters:
Let $G$ be the group of reduced residue classes modulo k. ($G = \mathbb Z/k\mathbb Z^\times$)
Coressponding to each character f of G, we define an arithmetic function $\chi = \chi_f$ as follows:
\begin{align*}
&\chi_f: \mathbb N (?) \rightarrow \mathbb C
&\chi_f(n) = 
 \begin{cases}
 f(\hat{n}) & \text{if $(n,k)=1$} \\
 0 & \text{if $(n,k)>1$ }
 \end{cases} \\
&\chi_1 \equiv \text{$1$ if $(n, k) = 1$ else $0$}
\end{align*}


#### Thm 6.15 
There are $\varphi(k)$ characters modulo k, each of which is completely multiplicative and periodic with period k.
i.e. 
$$
\chi(mn)=\chi(m)\chi(n)\\
\chi(n+k) = \chi(n)
$$

```
Commentary (bollu, crypt)
k = 3
n:     0, 1,        2, 3, 4, 5, 6, 7
chi n: 0, f(1),f(2),   0, f(1), f(2), 0
this cannot  have period less than 3
because f(_) is in nth roots of unity,
cannot become 0.
```





Conversly, if $\chi$ is completely multiplicative and preiodic with period $k$ and if $\chi(n) = 0~\text{if}~ (n,k) >1$ then $\chi$ is one of the dirichlet character modulo k.

#### Proof Thm 6.15 (forward): 
There are $\varphi(k)$ characters f of G, hence there are $\varphi(k)$ characters $\chi_f$ modulo $k$. The multiplicative property follows from f when both m,n are relatively prime to k. If one of them is not relatively prime, then neither is $mn$ hence both values become 0. 
The periodicity  property follows from the fact that $\chi_f(n) = f(\hat{n})$ and that $a\equiv b \mod k$ implies $\gcd(a,k) = \gcd(b,k)$.

#### Thm 6.15: Proof of converse
To prove the converse, we note that the function f defined on the group G by the equation:

$$
f(\hat{n}) = \chi(n)\ \text{if}~ (n,k)=1
$$

is a character of G, so \chi is a dirichlet character mod k.

The image of $\chi$ must be a root of unity because

\begin{align*}
&\chi(1 \times 1) = \chi(1) \implies \chi(1) = 1 \\
&\chi(a)^{\varphi(k)} = \chi(a^{\varphi(k)}) = \chi(1 \mod k) = \chi(1) = 1 \\
&\chi(a)^{\varphi(k)} = 1 \\
&\chi(a) = \text{$\varphi(k)$th root of unity}
\end{align*}


#### Thm 6.16


Let $\chi_1, \chi_2 \dots \chi_{\varphi(k)}$ denote the $\varphi(k)$ dirichlet characters modulo k.
Let m and n be two integers with $\gcd(n,k)=1$
Then we have:

$$
\sum_{r=1}^{\varphi(k)} \chi_{r}(m)\overline{\chi_r(n)} =
\begin{cases}
\varphi(k) & \text{if $m\equiv n \mod k$} \\
0 & \text{if $m \not \equiv n \mod k$}
\end{cases}
$$

##### Proof:

- If $\gcd(m,k)=1$ take $a_i = \hat{n}$ and $a_j = \hat{m}$ in the orthogonailty relation of theorem 6.12 and note that $\hat{m} = \hat{n}$ if and only if $ m\equiv n \mod k$.
- If $\gcd(m,k) > 1$ each term in the sum vanishes and 
$m \not \equiv n \mod k$.



### Sums Involving Dirichlet characters:

#### Thm 6.17
Let $\chi$ be any non-principal character modulo k, let $f$ be a non-negative function which has a continuous negative derivative $f'(x)$ for all $x \ge x_0$. Then if $y \ge x \ge x_0$ we have:

$$
\sum_{x<n\le y} \chi(n)f(n) = O (f(x)) - (7)
$$

If in addition $f(x) \rightarrow 0$ as $x \rightarrow \infty$ then the infinte series 

$$
\sum_{n=1}^{\infty} \chi(n)f(n)
$$

converges and we have for $x\ge x_0$,
$$
\sum_{n\le x} \chi(n)f(n) = \sum_{n=1}^{\infty}\chi(n)f(n) + O(f(x)) - (8)
$$
Proof: Let $A(x) = \sum_{n\le x} \chi(n)$. Since $\chi$ is non principal we have
$$
A(k) = \sum_{n=1}^{k} \chi(n) = 0
$$

Because sum of nth roots of unity is equal to 0 for n strictly greater than 1. $\chi$ evaluates to nth roots of unity plus some extra zeroes over `[1..k]`

By periodicity it follows that A(nk) = 0 for n=2,3.... hence $|A(x)| < \varphi(k)$ for all x. i.e A(x) = O(1).

Notice that we only need to care about the last unevaluated period. This period has to be of length less that $\phi(k)$. If it were equal to $\phi(k)$, the
sum over this would be $0$. Now in this last period, which we shift to $[0\dots \texttt{leftover}]$, we get:

$$
|A(n)| = 
\left| \sum_{n=0}^{\texttt{leftover}} \chi(n) \right| \leq 
\sum_{n=0}^{\texttt{leftover}} | \chi(n) |\leq
\sum_{n=0}^{\texttt{leftover}} 1 \lneq \varphi(k)
$$

Note that $|\chi(n)| \leq 1$ since $\chi(n)$ evaluates to either 0 or a $\varphi(k)$ th root of unity whose absolute value is 1. 

#### Chapter 4 thm 4.2:

**Abel's Identity**: For any arithmetical function $a(n)$ let
$$
A(x) = \sum_{n\le x} a(n)
$$

where $A(x) = 0$ if $x<1$. Assume $f$ has a continuous derivative on the interval $[y,x]$ where $0<y<x$. Then we have:

$$
\sum_{y<n\le x} a(n)f(n) = A(x)f(x) - A(y)f(y) - \int_{y}^{x} A(t)f'(t) dt
$$

##### Proof:
Let $k =[x]$ and $m=[y]$ so that $A(x)=A(k)$ and $A(y)=A(m)$.
Then:

\begin{align*}
&1. \sum_{y<n\le x}a(n)f(n) = \sum_{n=m+1}^{k} a(n)f(n) = \sum_{n=m+1}^{k}  \{A(n)-A(n-1)\}f(n)\\
&2.  = \sum_{n=m+1}^{k} A(n)f(n) - \sum_{n=m}^{k+1} A(n)f(n+1)\\
&3.  = \sum_{n=m+1}^{k-1} A(n)\{f(n)-f(n+1)\} + A(k)f(k) -A(m)f(m+1)\\
&4.  = - \left[ \sum_{n=m+1}^{k-1}A(n) \left ( \int_{n}^{n+1}f'(t)dt \right) \right] + A(k)f(k) - A(m)f(m+1)\\
&5.  = - \left[\sum_{n=m+1}^{k-1}\int_{n}^{n+1} A(t)f'(t)\right] + A(k)f(k) -A(m)f(m+1) \\
&6.  = -\int_{m+1}^{k} A(t)f'(t) + A(x)f(x) - \int_{k}^{x} A(t)f'(t)dt - A(y)f(y) - \int_{y}^{m+1} A(t)f'(t)dt\\
&6. = A(x)f(x) - A(y)f(y) - \int_{y}^{x}A(t)f'(t)dt
\end{align*}

- [Link to proof on wikibooks](https://en.wikibooks.org/wiki/Analytic_Number_Theory/Useful_summation_formulas)

##### Proof of (7) 

$$
\sum_{x<n\le y} \chi(n)f(n) = f(y)\chi(y) - f(x)\chi(x) - \int_{x}^{y} A(t)f'(t)dt \\
 =  O(f(y)) + O(f(x)) + O(\int_{x}^{y}A(t)f'(t)dt)\\
 = O(f(x))
$$

We note that

$$
\int_x^y A(t) f'(t) dt 
\leq \int_x^y |A(t)| f'(t) dt
\leq |1| \int f'(t) dt
\leq \int_x^y f'(t) dt = f(y) - f(x)
$$

by using the fact that (1) $f'(t)$ does not change sign, (2) $|A(x)| \leq 1$.

If $f(x) \rightarrow 0$ as $x \rightarrow \infty$ then eqn 7 shows that the series 
$$
\sum_{n=1}^{\infty} \chi(n)f(n)
$$
converges because of cauchy convergence (TODO) criterion. Mayhaps the proof is:

$$
\lim_{k \rightarrow \infty} \sum_{n=1}^{k} \chi(n)f(n) \lneq \phi(k) f(n)\\
\forall \epsilon > 0, \exists N, \forall n \geq N, |\phi(k) f(n+1) - \phi(k)f(n)| < \epsilon \\
$$

To prove eqn 8 we simply note that 
$$
\sum_{n=1}^{\infty}\chi(n)f(n) = \sum_{n\le x}\chi(n)f(n) + \lim_{y\rightarrow \infty} \sum_{x<n\le y} \chi(n)f(n)
$$
Because of eqn 7, the limit on the right if O(f(x)). This completes the proof.

Mow we apply thm 6.17 successively with f(x) = 1/x, f(x) = (log(x))/x and f(x) = 1/\sq
THm 6.18 
