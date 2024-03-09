# Discrete probability book

# Meta principle

- Rephrase every counting principle as the size of some set!

# Ch2: Counting

Find number of ways to pull out `k` objects / perform `k` draws from urn with `n` objects.

- order counts, with replacement: This is the size of the set of `k` tuples with entries in `[1..n]`: `n^k`
- order counts, without replacement: This is the size of the set of `k` tuples such that no two entries are equal: `nPk`
- order does not count, without replacement: This is the size of the set of `k` subsets of set `[1..n]`: `nCk`
- order does not count, with replacement: We need to figure out after `k` draws, how many of them were `1`, how many were `2`, ...,
  how many were `n`.
- We can count the above by considering `(n - 1)` bars, `k` dots, and we pick `k` locations to place the dots. So we have
  `(n-1+k)Ck`.

#### Order does not count, with replacement, case with `n=2`

- Suppose we have only two objects `1, 2` and we perform `k` draws from urn, with replacement. Furthermore, order does not count.
- So we have a collection of `k` objects. Let's standardize, and say that we place the `1` objects before the `2` objects:
  (since order does not count, we can pick a representative). Now we need to choose the split point of where we go from `1` to `2`.
- Suppose for concreteness that `k=3`. Then we have the cases:

```
111|
11|2
1|22
|222
```

- There are `(k + 1)` choices for this.

- From a combinatorial lens, there are the locations:

```
  x   y   z
_ * _ * _ * _ 
a   b   c   d
```
- we have 4 (`k+1`) locations, and we need to place `1` bar (which is `n-1`).




# Ch3: Independence and conditional probability

