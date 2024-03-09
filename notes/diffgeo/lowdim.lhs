\documentclass{book}
\usepackage[sc,osf]{mathpazo}   % With old-style figures and real smallcaps.
\linespread{1.025}              % Palatino leads a little more leading
\usepackage{hyperref}
\hypersetup{
    colorlinks,
    citecolor=blue,
    filecolor=blue,
    linkcolor=blue,
    urlcolor=blue
}
\usepackage{minted}
\usepackage{amsmath}
\usepackage{amssymb}
\newcommand{\R}{\ensuremath{\mathbb R}}
\newcommand{\Rn}{\ensuremath{\mathbb R^n}}
\newcommand{\Tp}{\ensuremath{T_p}}
\newcommand{\TpRn}{\Tp \Rn}

\newtheorem{theorem}{Theorem}
\newtheorem{corollary}[theorem]{Corollary}
\newtheorem{definition}[theorem]{Definition}
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{observation}[theorem]{Observation}
\newtheorem{proof}[theorem]{Proof}
\newtheorem{remark}[theorem]{Remark}
\newtheorem{example}[theorem]{Example}

\begin{document}
\chapter{Introduction}
- \href{https://www.youtube.com/watch?v=A2Z0K8bfsig&list=PLBY4G2o7DhF38OEvEImfR2heX7Szmq5Gs&index=2}{DiffGeo lectures by james cook}
\chapter{Lecture 2: Part 1}
\begin{definition}
Real space:
$$ \R^n \equiv \{ (p^1, p^2, \dots, p^n) : p^i \in \R \}$$
\end{definition}
\begin{definition}
Standard basis:
$$
(e_i)^j \equiv \delta_i^j = \begin{cases} 1 & i = j \\ 0 & \text{otherwise} \end{cases}
$$
\end{definition}


\begin{definition} Tangent space at point $Q \in \R^n$:
$$ T_Q \mathbb R^n \equiv \{ q \} \times \mathbb R^n $$
\end{definition}

Tagent space is a vector space structure. Just ignore the basepoint.

\begin{definition} Tangent bundle:
$$
T \R^n \equiv \bigcup{p \in \R^n} T_p \R^n = \bigcup{p \in \R^n} \{ p \} \times \R^n
$$
\end{definition}

\begin{definition} Vector field $X: S \rightarrow T \R^n$ if
$$ \forall p \in S, X(p) \in T_p \R^n $$
\end{definition}

\begin{definition}
Projection onto the tangent bundle:
$$
\pi: T \R^n \rightarrow \R^n; \pi((p, v)) \equiv p
$$
\end{definition}
If $X$ is a tangent bundle then $\pi \circ X = id_S$. That is, $X$ is a section of the
tangent bundle.

\chapter{Lecture 2: Part 2}

\begin{definition} Coordinate functions:
$$
X^i: \R^n \rightarrow \R: X^i((p^1, \dots, p^n)) \equiv p^i
$$

In $\R^3$ we write $x = X^0, y = X^1, z = X^2%.
We can use this to sneakily build functions. For example: $f = x + yz$ then
$f$ really is a function $f: \R^3 \rightarrow \R$ since the space of functions
$\R^3 \righarrow \R$ is a ring. There's no sleight of hand or abuse of notation!

\begin{definition}
Directional derivatives: Let $: S \subseteq \Rn \rightarrow R$. Let $p \in S$.
Then the directional derivative of $f$ at $p$ with respect to $(p,v) \in  \TpRn$
is defined as:

$$
D : (\Rn \to R) \times \TpRn \to \R
(D f)(v)(p) = (D f)((p, v)) = \lim_{t \to 0} \frac{f(p + tv) - f(p)}{t}
$$

If we define $\alpha(t) = p + tv$ then we can write the above as:

$$
(D f)((p, v)) = \lim_{t \to 0} \frac{(f(\alpha(t)) - f(\alpha(0))}{t} = \frac{d (f \circ \alpha)(t)}{dt}|_{t = 0}
= \sum_{i = 1}^n  \frac{\partial f}{\partial X^i}(\alpha(0))  ...
TODO: fill this computation in. (7:40)
$$

This computation shows us that

\begin{aligned}
 (D f)((p, v)) \equiv \sum_{i=1}^n v^i \frac{\partial f}{\partial X^i}|_p
\end{aligned}

So, we choose to \textbf{redefine tangent vectors}!

\begin{definition} Tangent vectors, the real definition:

$$
\Tp \Rn \equiv \left \{   \sum_{j=1}^n v^i \frac{\partial}{\partial X^i}|_p \right \}
$$
\end{definition}

\begin{definition} Derivation:
A derivation is an element of the tangent space $X \in \Tp \Rn$. So
$X: (\Rn \to \R) \to \R$. So it is
linear:

$$
X(f + g) = X(f) + X(g); X(cf) = c X(f)
$$

and obeys the product rule:
$$
X(fg) = X(f)(p)g(p)+ f(p) X(g)
$$
\end{definition}

\begin{definition}
Dual space of the tangent space:

$$
d_p  f (v_p) \equiv v_p [ f]
$$

Here the elements of the form $d_p f$ are a basis for the cotangent space
where $f \in \Rn \rightarrow R$

\end{document}
