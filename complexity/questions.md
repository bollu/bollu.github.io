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

[2] 1.15 (pg 33)
Show that choosing a different base of representation will make no difference to the class P.

Show that choosing the unary representation may make a difference by showing that the
following language is in P: UNARYFACTORING = (n, l, k) unary, there is a prime in j \in (l, k) dividing n

[3] 2.8 (pg 59)
HALT = 
show that HALT is NP hard. is it NP complete?

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
Show that there is a language in EXP such that P^B != NP^B.

[8] 3.5 (pg 69)
Show that there exists a function that is non time constructible

[9] 4.4 (pg 83)
Show that following language is NL complete: G: G is a strongly connected digraph


[10] 4.5 (pg 83)
Show that 2SAT is in NL.
Convert 2SAT to implication graph, now check for cycles.

[11] 5.3 (pg 92)
Show that if 3SAT is a polynomial time reducible to 3SATbar, then PH = NP.

[12] 5.10 (pg 93)
Let A be a language such that P^A = NP^A. Then show that PH \subset P^A

[13] 6.3 (pg 107)
Q. Describe a decidable language in P/Poly that is not in P

A. Make it do super exponential amount of work: 
{ 1^n, n halts in 2^2^n steps }

[14] 7.10 (pg 124)
Show that random walk does not work for connectivity on directed graphs.

[15] 8.10 (pg 150)
Show that GI is downward self reducible. Given the ability to solve GI on (n - 1)
graph, solve it on n vertex graph.

[16] 9.5 (pg 171)
Show that if P=NP, one way functions do not exist.


Let a one way function exist. We can invert it using NP. But P = NP, so we can
do so deterministically, hence OWF do not exist.

[17] 9.11 (pg 172)
if f is a one way permutation, so is f^k

[18] 9.12 (pg 172)
assuming one way functions exist, so that f^k need not be a one way function.

[19] 10.6 (pg 204)
Q. Take a 2 qubit system in an arbitrary state. show that the following all yield
the same answer:
1. measure the register then output
2. first measure the 1st qubit and output it, then measure the 2nd qubit and output it
3. first measure the 2st qubit and output it, then measure the 1st qubit and output it


[20] 10.16 (pg 204)
suppose `j, r <= N` are mutually coprime, and are unknown to us. Show that
if the first `2 log N` bits are known of `j, r`, we can then regain it in 
poly time.
