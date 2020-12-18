

- Schur polynomials: \sum_[T in SSYT(N, m/n)] x^[content[T]]
- semistandard tableaux (increases in col, does not decrease in row, contains entires from {1..N}) 
- standard: contain each number in 1..n exactly once
- standard tableaux (increases in col, increases in row, contains entires from {1..N}) 

- power sums: p[k] = sum[i] x_i^k
- elementary symmetric polynomials: e[k](x1,...xn) = sum over all k-tuples of [x_1...xn] WITHOUT repetition
  eg: e[2](x1, ... x5) = x1x2 + x2x3+  ... + x4x5
- complete symmetric polynomials: h[k](x1, ... xn) = sum over all k-tuples of [x1..xn] WITH repetition
  eg: e[2](x1...x5) = x1^2 + x2^2 + ... x5^2 + 



:  IF: (i1=1, i2=1, i3=2)  x1^2 x2
: THEN x1 x2^2 (i1=1, i2 = 2, i3 = 2)

- we're saying: take ALL possible ways with repetition.
- of course this will include all possibele ways to take "permuted".

- We are picking {1...N} with repetition $k$ times
- (a1, a2, .. an) | a1 + a2 + ... + an = k
- a[σ1], a[σ2]... a[σn] | aσ1 + aσ2 + ... + an = k [σ is a permutation]


- N = 8
- mu: 5 >= 2 >= 1


- x1^5 x2^2 x3 + x2^5 x3^2 x4 + ... + x8^1 x7^5 x6^2

- mu = mu[1] >= mu[2] >= ... >= mu[n].
- monominal symmetric polynomial indexed by mu: Informally whose ex-ponent vector can be rearranged to give μ.

let i = σ-1[j]

- f[1] = 5; f[2] = 3; f[3] = 1;
x[1]^f[1] x[2]^f[2] x[3]^f[3] = x1^5 x2^3 x3

σ 1 <-> 2

x[σ[1]]^f[1] + x[σ[2]]^f[2] + x[σ[3]]^f[3]

x[1]^f[σ-1[1]] + x[2]^f[σ-1[2]] + x[3]^f[σ-1[3]]

g[1] = f[σ-1[1]] 
g[2] = f[σ-1[2]] 
g[3] = f[σ-1[3]] 


x[1]^g[1] x[2]^g[2] x[3]^g[3]

sort(f) = sort(g) = [5, 3, 1]
- Λn = all symmetric polynomials x1...xn


- The symmetric polynomials eα hα pα: let α be a sequence of [psotive integers (α1, ... αn)
  eα = prod_i eαi


x1, x1^2, x2, x2^2, ... x_k -> basis
schur polynomials -> basis


- homogenous stuff.
- homogenous poly: all terms of the same degree. x1^2 + x2^2: homo. x1 + x2^2 NOT
- homogenous poly of degree k: form a vector space: (intuition: adding and scaling don't change degree)
- some symmetric polynomial:
- p(x) = x1x2^2 + x2x1^2 + x1x2 + x2x1 + x1 + x2
- p_1(x) = x1 + x2  ∈ Λ_2[1]
- p_2(x) = x1x2 + x2x1  ∈ Λ_2[2]
- p_3(x) = x1x2^3 + x2^2x1  ∈ Λ_2[3]
- p = p1 + p2 + p3
