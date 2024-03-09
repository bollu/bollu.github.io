# No free lunch theorem

> the hypothesis class of all functions is not PAC learnable

#### Theorem: 

Let $A$ be any learning algorithm (binary classification) with
domain $X$ (labels come from $Y \equiv \{1, 2\}$).
If the number of samples given is less than or equal to $|X|/2$,
then for any learning algorithm $A$, there exists a function $f$ and a distribution
$D$ such that:

1. $L_D(f) = 0$ (the function perfectly labels the samples).
2. $Pr_{S \sim D^m} [L_D(A(s)) \geq 1/8] \geq 1/7$.

This is a violation of the PAC guarantee, which needs a guarantee of
the form $\forall \epsilon, \delta$ $Pr_S \sim D^m}[L_D(A(S)) > \epsilon] < \delta$.
But here, we cannot choose $\epsilon, \delta$ as small as we want.

#### Proof:

Let us assume that $|X| = 2m$. The algorithm $A$ sees $m$ samples. The number
of functions from $X \rightarrow \{0, 1\}$ is $2^|X| = 2^{2m} \equiv T$. 
Let these functions be $f_1, f_2, \dots, f_T$. Let each of these functions
have a cooresponding distribution $D_i: X \times \{0, 1\} \rightarrow \mathbb R$,
which are defined as:

$$
D_i((x, y)) = \begin{cases} 
                    \frac{1}{2m} & \text{if f_i(x) = y}
                    0 & \text{otherwise}
              \end{cases}
$$

That is, it allows us to uniform sample over those $x_0 \in X$ such that $f_i(x_0) = y_0$.
So the distribution $D_i$ of $f_i$ picks those samples $x_0$ where $f_i$ is right.

We will prove that there is some function $f_i$ which has high generalization
error. This essentially arrives from markov's inequality.

$$
\max_{i \in [1\dots T] \mathbb E_{S \sim D_i^m} [ L_{D_i}(A(s)) ] \geq \frac{1}{7}
$$

If we look at the marginal of $D_i$ on $X$, it's going to be uniform over those
values that $f_i$ label correctly.

$S \equiv (x_1, x_2, \dots, x_m)$. 

$$
\mathbb{e}_{s \simeq d_I^M} L_{D_I}(A(S)) = \frac{1}{k} \sum_{J=1}^K L_{D_i}(A(S_j^i)
$$

Recall that the maximum value is at least equal to the average value:

$$
\max_{i \in [1\dots T] 
\mathbb{E}_{S \simeq D_i^m} L_{d_i}(A(S)) \geq \frac{1}{T} \sum_{i=1}^T L_{D_i}(A(S_j^i)$
$$

Let us consider the set of failures:
$$
\sum_{i=1}^T L_{D_i}(A(S_j^i)) =  \sum_{i=1}^T 1[A(S_i^j) \neq f_i(x)]
$$


<!--We will denote $S^i_j \equiv \{ (x_1, f_j(x_1)), \dots, (x_m, f_j(x_m) \}$ will-->
