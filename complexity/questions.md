Questions from aroara barak compexity theory, first edition.

[1] 0.2 (pg 10)
(a) f (n) = f (n − 1) + 10.
(b) f (n) = f (n − 1) + n.
(c) f (n) = 2f (n − 1).
(d) f (n) = f (n/2) + 10.
(e) f (n) = f (n/2) + n.
(f) f (n) = 2f (n/2) + n.
(g) f (n) = 3f (n/2).
(h) f (n) = 2f (n/2) + O(n^2 ).


A.
a) f(0) = 1; f(1) = 1 + 10; f(2) = 1 + 2*10; f(n) = 1 + 10*n
b) f(0) = 1; f(1) = 1 + 1; f(2) = 1 + 1 + 2; f(n) = 1 + n (n+1)/2
c) f(0) = 1; f(n) = 2^n;
d) f(n) = 10[n] + 10[n/2] + 10[n/4] + .. + 10[i] = `10 log n`
e)f(n) = n + n/2 + n/4 + ... =  n(1 + 1/2 + 1/4 + ... + 1/2^k)
f) f(n) = 2f(n/2) + 3 = 4f(n/4) + 6 = 8f(n/8) + 9 = 16f(n/16) + 12 = 2^k f(n/(2^k)) + 3k = 2^(log n) + 3 (log n)
g) f(n) = 3f(n/2) = 3 * 3* f(n/4) = 3^kf(n/2^k) = 3^log n
h) f(n) = O(n^2) + 2(O(n/2)^2) + 4 (O(n/4)^2) + .. = O(n^2)

[2] 1.15 (pg 33)
Q. 1. Show that choosing a different base of representation will make no difference to the class P.

2. Show that choosing the unary representation may make a difference by showing that the
following language is in P: UNARYFACTORING = (n, l, k) unary, there is a prime in j \in (l, k) dividing n

A. 
1. we can spend poly time converting the new representation to base 2.
2. since each number is encoded in unary, we get O(n) time, where n is the
number. So, we can search through the candidates, solve the division etc
in log n time, and then output the result.



[3] 2.8 (pg 59)
HALT = take `<a, x>`. Return 1 if `M_a, x` halts, 0 otherwise. `M_a` is the
the string `a` interpreted as a machine.

show that HALT is NP hard. is it NP complete?

A. reduce 3SAT to HALT. Encode a machine that tries all possible assignments.
Halts if it solves, does not halt if it does not. invoke HALT.

[4] 2.10 (pg 59)
Q.Let L1, L2 be in  NP. What about (L1 union L2)? (L1 intersection L2)?

A. For L1 intersection L2, solve both problems, and if both return true
   return true.

   For L1 union L2, try both problems, and if either returns true, return true.


[5] 2.25 (pg 60)
Q.Prove that if P = NP then NP = coNP.

A.NP = P => co-NP = co-P, but co-P = P since we can run the computation and
flip the bit.

[6] 3.2 (pg 69)
Show that SPACE(n) != NP.

NP is closed under PTIME reduction.

SPACE is not closed under PTIME reduction. 

Consider a problem in SPACE(n^2). Given an input |w|, |w| = n
pad it by adding additional data into the string, 
so that it can then be solved in SPACE(|w 0^k| = n^2|). that is, |0^k| = n^2 -
1, or k = n^2 - 1.


[7] 3.3 (pg 69)
Q. Show that there is a language in EXP such that P^B != NP^B.

A. The BGS proof.

Consider a languge EXP_L = { 1^n, there exists a string w ∈ L, |w| = n}.

Call O the decider oracle for langauge L

Clearly, this language can be solved by NP^O, by guessing a string w and then
checking if O(w) = 1.

What we want to do is to fill up the language L inductively, such that whatever
query P^L makes, we will thwart it by filling in new things into the language L.

We create L in stages, with L_0 = 0, and L_i designed inductively.

Step for L_i:
- Consider the machine M_i, which is i interpreted as a machine.
- Let n be the largest string in L so far. Invoke `M_i(2^n)`
- If M_i asks about a string that was in L, answer truthfully. If M_i
asks about strings that are of length `2^n`, answer NO. 

Note that `M_i` must run in poly time, and can thus only sample polynomial
number of strings from the space of `2^n`. Thus, it cannot check each string
of length `2^n`. So, we can tell it that no string belongs to `2^n`, and then
change whether a string of that length belongs to `L` _after_ M has answered.

- Once M_i has finished, if M_i answers `YES`, meaning that the language `L`
contains a string of length `2^n`, then don't add any such string into the language.
We can be sure that we will not add such strings into the language in the future,
since we will never consider the length `2^n` again.

- If `M_i` answers `NO`, that is, the language `L` does not contain any string of 
length 2^n, add the string `0^{2^n}` into `L`. Thus, `M` fails to notice that
the string `0^{2^n}` ∈ `L`, and hence `M` cannot solve `EXP_L`

if M_i answers `YES`, that is, the language `L` does contain strings of length `2^n`,
do not add any string into the language. Thus, `M_i` believes incorrectly that the
language contains a string of length `2^n`. So, `M` cannot decide `EXP_L`.



[8] 3.5 (pg 69)
Q. Show that there exists a function that is non time constructible

A. Create an undecidable function.

[9] 4.4 (pg 83)
Q. Show that following language is NL complete: G: G is a strongly connected digraph

A. 1. STRONGLYCONNECTED ∈ NL. Pick two vertices, check PATH on them. If PATH
returns 0, return FALSE. retur TRUE if we exhaust all vertices. only need 2
pointers into V.
A. 2 reducing PATH to STRONGLYCONNECTED. On asked PATH(G, s, t), make a new graph
G' = G, and for each i in V, add edges `i->s`, `t->i` into G'. 

- Now, if `s` and `t` HAVE a path between them, there exists a path for any two vertices `i`
`j` as `i->s->t->j` and `j->s->t->i`

- if `s` to `t` does NOT have a path, then the graph `G'` will not be strongly
conneced, since:
    - G was not (if it was, there would be a path from `s` to `t`, contradiction)
    - we only added paths from `i->s` and `t->i`, so there cannot be new ways
    from `s -> t`, because we only added `*->s` and `t->*`, NOT `s->*->t`.


[10] 4.5 (pg 83)
Q. Show that 2SAT is in NL.

A. Convert 2SAT to implication graph, now check for cycles. Use bar(PATH). If 
there are no cycles, then output 1. (TODO: flesh out details)

[11] 5.3 (pg 92)
Q. Show that if 3SAT is a polynomial time reducible to 3SATbar, then PH = NP.

A. Since 3SAT is NP complete, and bar(3SAT) is coNP-complete, we can 
say that NP = co-NP. Now, by induction, PH of each level collapses.

Assume PH collapses for level i. Level i+1,
- let prob = exists x1, (forall x2, exists x3, ... M(x1, x2, ..., xn)). prob ∈ SIGMA_{i+1}
- let prob'(x1) = forall x2, exists x3, .. prob(x1, x2, ..., xn)
- but then prob'(x1) ∈ PI_i. Hence, by induction hypothesis, prob' ∈ NP.
- That means that we can write it prob'(x1) = exists x', M'(x1)(x')

- prob = exists x1, prob'(x1)
- prob = exists x1, exists x', M'(x1)(x')
- prob = exists (x1, x'), M'(x1)(x')
- prob ∈ NP
- SIGMA_{i+1} ∈ NP.
- Similar for PI_{i+1}.


[12] 5.10 (pg 93)
Q. Let A be a language such that P^A = NP^A. Then show that PH \subset P^A

A. Induction of PH. assume it holds till level (i - 1). That is, SIGMA_i \subset P^A, PI_i \subset P^A.

- PROB = E x1, FA x2, E x3, ... M(x1, x2, ... , xn), PROB ∈ SIGMA_{i+1}
- PROB'(x1) = FA x2, E x3, ... M(x1, x2, ..., xn). PROB'(x1) ∈ PI_i = P^A
- PROB' ∈ P^A => exists M'. M'(m1, x2, ... xn) solves the problem in P with queries to A.
- PROB = E x1, M'(m1, m2, ... xn) = NP^A = P^A.


[13] 6.3 (pg 107)
Q. Describe a decidable language in P/Poly that is not in P

A. Make it do super exponential amount of work: 
{ 1^n, n halts in 2^2^n steps }

[14] 7.10 (pg 124)
Show that random walk does not work for connectivity on directed graphs.

Take a graph like this:

```
cycle<--------------------------------*
^     ^     ^     ^     ^ ...         |
|     |     |     |     | ...         |
x1--> x2--> x3--> x --> x --> ... --> xn
```

TODO

[15] 8.10 (pg 150)
Show that GI is downward self reducible. Given the ability to solve GI on (n - 1)
graph, solve it on n vertex graph.

[16] 9.5 (pg 171)
Q. Show that if P=NP, one way functions do not exist.


A. Let a one way function exist. We can invert it using NP. But P = NP, so we can
do so deterministically, hence OWF do not exist.

[17] 9.11 (pg 172)
Q. if f is a one way permutation, so is f^k

A. Assume f^k is not a one way permutation. That means we can get (f^{-1})^k.
Apply f (k-1) times to get f^{-1}. Use this to invert. Hence, f is not a
one way permutation.

[18] 9.12 (pg 172)
Q. assuming one way functions exist, so that f^k need not be a one way function.

TODO

[19] 10.6 (pg 204)
Q. Take a 2 qubit system in an arbitrary state. show that the following all yield
the same answer:
1. measure the register then output
2. first measure the 1st qubit and output it, then measure the 2nd qubit and output it
3. first measure the 2st qubit and output it, then measure the 1st qubit and output it

A. let arbitrary state S = a00 + b01 + c10 + d11.  Assume a+b+c+d=1 WLOG

1. Probability of each state is {a, b, c, d}. 
2. Measure first qubit. Probaility of 1st qubit being 0 is a+b. New state is
    (a+b)0 + 1) + (c+d)(1


[20] 10.16 (pg 204)
suppose `j, r <= N` are mutually coprime, and are unknown to us. Show that
if the first `2 log N` bits are known of `j, r`, we can then regain it in 
poly time.
