A math.se quesstion, where I am trying to build intuition for myself: https://math.stackexchange.com/questions/3776438/intuition-for-fractions-of-the-localization-of-a-non-integral-domain


##### Fractions

Let me try and sketch out the setting here, in case this catches your fancy
[and is a nice rubber duck for me].

Let's say all we have are the integers `Z`, and we need to build the rationals `Q`.
How do we do it?

The general idea is we build _all_ fractions `(a, b)` where `a, b in Z`. We 
then "equate" fractions which are the same in their lowest common terms:
`a/b ~ c/d`. Formally, we setup an equivalence relation where `a/b ~ c/d`
if an only if `ad = bc`. [their cross ratio is equal]. 

#### Fractions: equivalence relation

Let's show that this is indeed an equivalence relation. Reflexivity and
symmetry are immediate from the equation `ad = bc`. The complicated
case is transitivity. 

Let us say we have `a/x ~ b/y`: `ay = bx`, and `b/y ~ c/z`: `bz = cy`. We
now want to show that `a/x ~ c/z`: `az = cx`. This is not so hard. We
proceed as follows:

```
ay = bx
bz = cy

(ay)z = (bx)z = x (bz) = xcy = y(cx)
(az)y = y(cx)
(az - cx)y = 0
```

Since we have that:
- `y != 0` because the denominators cannot be equal to zero
- Over the integers, if `pq = 0` then `p = 0` or `q = 0`,

we need `az - cx = 0`.


##### Fractions over zero divisors

Let's try to generalize to cases where we can have _zero divisors_ /
_divisors of zero_: numbers `p, q` such that `pq = 0` when both `p` and `q`
are not equal to `0`. For example, consider the integers modulo `10`,
written as `R = Z/10Z`. Here, we have elements `p=5`, `q=2` where `pq=10=0`,
but `p != 0` and `q != 0`.


##### Denominators for `R = Z/10Z`

Can we have all numbers in `Z/10Z` as the denominator? Let's try 
setting `5` as the denominator "naively" and see what happens. We get:

```
1/5 = 1/5 * 2/2 = 2/10 = 2/0
```

this gives us a division by zero. So what? Maybe it all works out. Let's
proceed:

```
2/0 = (2/2) / (0/2) = 1/0
```

This lets us show that

```
2/0 = 1/0 => 2 = 1
```

Clearly, something is pretty awry, or everything  has collapsed in some
horrendous way. Of course, all of this is performing "naive" manipulation.
So let's try to be rigorous.

##### Denominators for `R = Z/10Z`, take 2

- We continue to define fractions as element `a/b`, `c/d`. Two fractions
  are equivalent `(a/b ~ c/d)` if we have that `ad - bc = 0`. This is an equivalence
  relation from the above discussion.

- Let's have two elements `p, q` such that `p != 0`, `q != 0` such that `pq = 0`.
  For example, we can have `p = 5`, `q = 2`, such that `pq = 10 = 0`.
- Let's assume we are allowed to build a ratio `1/p`.
- Now let's try to find a ratio `x/y` which is equivalent to `1/p`.

```
1/p ~ x/y
1y - px = 0
```

Setting `x = q` gives:

```
1y - pq = 0
1y - 0 = 0
1y = 0
y = 0
```

This means that by our notion of "equivalence of frations", we have that
`1/p ~ q/0`. 


We want to derive a contradiction. We have that `1/p ~ q/0`. Let's try to show
that `q/0 ~ 0/p`, thereby getting that `1/p ~ 0/p`, and hopefully, `1 = 0`.



Consider `q/0 ~? 0/p`:

```
q/0 ~? 0/p

q*p - 0*0 =? 0
0 - 0 =? 0
0 = 0
```

Hence it is indeed true that `1/p ~ q/0 ~ 0/p`. This means that we have:

```
1/p ~ 0/p
1*p - p*0 = 0
p(1 - 0) = 0
```

So we need either `p = 0` or `1 = 0`. We started with the assumption that `p != 0`.
Hence, `1 = 0`.


#### What are good choices of denominators?

- From the above, it should hopefully be somewhat intuitive that if we choose
  elements which are divisors of zero as the denominator, we get `0 = 1`,
  and thus everything collapses.

- Let's re-analyze what we needed from the numerator and denominator when
  trying to define the usual fractions. Let's say that the original ring is `R`,
  and the fractions come from some subset `S`.

- First of all, we need some way to interpret elements of `R` as fractions.
  The natural idea is to consider `r in R` as `r/1`. For this, we need `1 in S`.

- Next, we want to be able to define addition and multiplication. We need:

```
r/s + r'/s'
= (s'r + r's) / (ss') 
= r'' / s''

r/s * r'/s'
= (rr') / (ss')
= r'' / s''
```

So we need (i) multiplication between `S` and `R` to be an `R` element,
and `(ii)` multiplication between `S` and `S` to be an `S` element.

- Property (i) will always be the case, because multiplying `R` and `R`
  will give `R`. `S` is a subset of `R`. So multiplying `S` and `R` will
  give `R`.

- Property  (ii) need not always happen. We need to choose our `S` such that
  multiplying `S` and `S` remains in `S`.


- We can define "fractions" as long as the set `S` is a multiplicative
  monoid. So `(S, x, 1)` forms a monoid.

#### What do we need to define fractions?

Now that we have this more "abstract" perspective, let's redo our proof
that the definition of equivalence of fractions, `a/x ~ b/y` iff `ay = bx`
is really an equivalence relation.

- We have that `a/x ~ b/y`: `ay = bx`.
- We have that `b/y ~ c/z`: `bz = cy`.

```
(ay)z = (bx)z = x (bz) = xcy = y(cx)
(az)y = y(cx)
(az - cx)y = 0
```

- We're stuck at this stage. From `(az - cx)y = 0`, we can't conclude that
  `az - cx = 0`, because our abstract ring may have zero divisors. 

- The solution is to _hack_. We weaken our equivalence relation. We say that
  `a/s ~ a'/s'` iff there exists an `y in S` such that `(as' - a's)y = 0`.

- This subsumes our previous definition, because we can set `y = 1` to
  regain the original case.

- This also allows for new cases like the above to work out, where we can have
  the `y` be something not equal to `1`, and `(as' - a's)` be something
  other than `0`, both multiply to give a `0`.


##### Localization

Congragulations, we're re-invented the idea of a
[localization of a ring](https://en.wikipedia.org/wiki/Localization_of_a_ring).
Let's now study the localization of one ring in detail.

##### Localizing `Z/10Z` at `6`.

to localize, recall that we need to choose a ring `R` and a subset `S` which
is a monoid. We'll choose the powers of `6`. We have `6^0=1`, `6^1=6`, 
`6^2=36`. Buy `36 = 6 (mod 10)` so we're done, and our `S = {1, 6}`.



##### Fancy description

Given a ring R, if we localize at some `r in R`, then anything in the ring
which is annhilated by `r` becomes zero in the localization, because if
`rm = 0`, then `m/1 = 0/1`.

In the case of `Z/10Z`, note that `6` annhilates `{0, 5}`, and we thus
get the relations `0 = 0`, and `5 = 0`. Everything else follows from this.


##### SAGE code

```py
import itertools
# check all pairs (a, c in R | b, d in S):
#      (a/b) ~ (c/d)
# check exists e in S: (ad - bc)e = 0
R = IntegerModRing(10)

# Multiplicative subset to localize at
S = []
# Element that generates the multiplicative subset
s0 = R(6)

for i in range(0, 10000):
    if s0 ** i in S: break
    S.append(s0**i)

print("ring: %s" % R)
print("localizing at s0=%s | S = %s" % (s0, S))


equiv_relation = {}
for (a, b, c, d) in itertools.product(R, S, R, S):
    # break early, so we get that (a*d-b*c)*s = 0 for simpler s
    # [lower powers] than higher powers.
    for s in S:
        if (a * d - b * c) * s == R(0):
            equiv_relation[(a, b, c, d)] = s
            break

# compute equivalence classes
fracs = set(itertools.product(R, S))
equiv_classes = [] # (representative, set)

while fracs:
    # representative
    (a, b) = fracs.pop()
    cls = set([])
    cls.add((a, b))
    for (c, d) in fracs:
        for s in S:
            if s*(a*d - b*c) == 0:
                cls.add((c, d))
                break

    for frac in cls:
        if frac in fracs: fracs.remove(frac)

    # try to find a better representative that has denominator 1
    for (c, d) in cls:
        if d == 1:
            (a, b) = (c, d)
            break
    equiv_classes.append(((a, b), cls))

print("===Equivalence classes===")
for (rep, cls) in equiv_classes:
    print(rep, cls)

# check that all intersections are empty
for ((_, es), (_, fs)) in itertools.product(equiv_classes, equiv_classes):
    assert(es.intersection(fs) == es or es.intersection(fs) == set())

# check that the equivalence classes are exhaustive
for (r, s) in itertools.product(R, S):
    found = False
    for (_, cls) in equiv_classes:
        if (r, s) in cls: found = True
    if not found:
        raise RuntimeError ("element %2s/%2s not in any equivalence class" % (r, s))


# print how each representative relates to the other elements:

for ((a, b), cls) in equiv_classes:
    print("****%s/%s****" % (a,b))
    for (c, d) in cls:
        s = equiv_relation[(a, b, c, d)]
        out = "  - %2s/%2s (~ %2s)" % (c, d, s)
        out += " [mod %s]" % (R.order())
        out += "\n    "
        out += "(%2s*%2s - %2s*%2s) * %2s" % (a, d, b, c, s)
        out += " = "
        out += "(%2s - %2s) * %s" % (a*d, b*c, s)
        out += " = "
        out += "%2s * %2s" % (a*d - b*c, s)
        out += " = %s" % ((a*d - b*c)*s)
        print(out)
```

##### localizing at `0`:

Using the code, we can localize at different elements. 
On localizing at `0`, we everything collapses into a single equivalence
class of `0/1`. The text below shows how everything is in the
same class.

```
===Equivalence classes===
****0/1****
   0/ 1 (~  1) [mod 10]
      ( 0* 1 -  1* 0) *  1 = ( 0 -  0) * 1 =  0 *  1 = 0
   9/ 0 (~  0) [mod 10]
      ( 0* 0 -  1* 9) *  0 = ( 0 -  9) * 0 =  1 *  0 = 0
   0/ 0 (~  1) [mod 10]
      ( 0* 0 -  1* 0) *  1 = ( 0 -  0) * 1 =  0 *  1 = 0
   7/ 0 (~  0) [mod 10]
      ( 0* 0 -  1* 7) *  0 = ( 0 -  7) * 0 =  3 *  0 = 0
   9/ 1 (~  0) [mod 10]
      ( 0* 1 -  1* 9) *  0 = ( 0 -  9) * 0 =  1 *  0 = 0
   7/ 1 (~  0) [mod 10]
      ( 0* 1 -  1* 7) *  0 = ( 0 -  7) * 0 =  3 *  0 = 0
   8/ 1 (~  0) [mod 10]
      ( 0* 1 -  1* 8) *  0 = ( 0 -  8) * 0 =  2 *  0 = 0
   6/ 1 (~  0) [mod 10]
      ( 0* 1 -  1* 6) *  0 = ( 0 -  6) * 0 =  4 *  0 = 0
   3/ 1 (~  0) [mod 10]
      ( 0* 1 -  1* 3) *  0 = ( 0 -  3) * 0 =  7 *  0 = 0
   6/ 0 (~  0) [mod 10]
      ( 0* 0 -  1* 6) *  0 = ( 0 -  6) * 0 =  4 *  0 = 0
   2/ 1 (~  0) [mod 10]
      ( 0* 1 -  1* 2) *  0 = ( 0 -  2) * 0 =  8 *  0 = 0
   3/ 0 (~  0) [mod 10]
      ( 0* 0 -  1* 3) *  0 = ( 0 -  3) * 0 =  7 *  0 = 0
   5/ 0 (~  0) [mod 10]
      ( 0* 0 -  1* 5) *  0 = ( 0 -  5) * 0 =  5 *  0 = 0
   5/ 1 (~  0) [mod 10]
      ( 0* 1 -  1* 5) *  0 = ( 0 -  5) * 0 =  5 *  0 = 0
   8/ 0 (~  0) [mod 10]
      ( 0* 0 -  1* 8) *  0 = ( 0 -  8) * 0 =  2 *  0 = 0
   2/ 0 (~  0) [mod 10]
      ( 0* 0 -  1* 2) *  0 = ( 0 -  2) * 0 =  8 *  0 = 0
   1/ 0 (~  0) [mod 10]
      ( 0* 0 -  1* 1) *  0 = ( 0 -  1) * 0 =  9 *  0 = 0
   4/ 1 (~  0) [mod 10]
      ( 0* 1 -  1* 4) *  0 = ( 0 -  4) * 0 =  6 *  0 = 0
   1/ 1 (~  0) [mod 10]
      ( 0* 1 -  1* 1) *  0 = ( 0 -  1) * 0 =  9 *  0 = 0
   4/ 0 (~  0) [mod 10]
      ( 0* 0 -  1* 4) *  0 = ( 0 -  4) * 0 =  6 *  0 = 0
```

##### localizing at `1`:

On localizing at `1`, nothing happens. All the elements of `Z/10Z`
stay as elements `r/1`. These are the two "extremes".

```
ring: Ring of integers modulo 10
localizing at s0=1 | S = [1]
===Equivalence classes===
****0/1****
   0/ 1 (~  1) [mod 10]
      ( 0* 1 -  1* 0) *  1 = ( 0 -  0) * 1 =  0 *  1 = 0
****9/1****
   9/ 1 (~  1) [mod 10]
      ( 9* 1 -  1* 9) *  1 = ( 9 -  9) * 1 =  0 *  1 = 0
****7/1****
   7/ 1 (~  1) [mod 10]
      ( 7* 1 -  1* 7) *  1 = ( 7 -  7) * 1 =  0 *  1 = 0
****8/1****
   8/ 1 (~  1) [mod 10]
      ( 8* 1 -  1* 8) *  1 = ( 8 -  8) * 1 =  0 *  1 = 0
****6/1****
   6/ 1 (~  1) [mod 10]
      ( 6* 1 -  1* 6) *  1 = ( 6 -  6) * 1 =  0 *  1 = 0
****3/1****
   3/ 1 (~  1) [mod 10]
      ( 3* 1 -  1* 3) *  1 = ( 3 -  3) * 1 =  0 *  1 = 0
****2/1****
   2/ 1 (~  1) [mod 10]
      ( 2* 1 -  1* 2) *  1 = ( 2 -  2) * 1 =  0 *  1 = 0
****5/1****
   5/ 1 (~  1) [mod 10]
      ( 5* 1 -  1* 5) *  1 = ( 5 -  5) * 1 =  0 *  1 = 0
****4/1****
   4/ 1 (~  1) [mod 10]
      ( 4* 1 -  1* 4) *  1 = ( 4 -  4) * 1 =  0 *  1 = 0
****1/1****
   1/ 1 (~  1) [mod 10]
      ( 1* 1 -  1* 1) *  1 = ( 1 -  1) * 1 =  0 *  1 = 0
```

##### Localizing at `3`:

Localizing at `3` keeps the same fractions as localizing by `1`. what
winds up happening is that since `3` is invertible now, we get "fun" results
like `5 = 5/7`. We need to remember that:

- `3^3 = 27 = 7 (mod 10)`
- `3^4 = 81 = 1 (mod 10)`

This lets us show that `5/7 = 5`:

```
5/7 
= 5/3^4 
= (5*3)/(3^5)
= 15
= 5
```

Since `3` is a unit, we don't "lose" elements when we localize.


```
****5/1****
   5/ 9 (~  1) [mod 10]
      ( 5* 9 -  1* 5) *  1 = ( 5 -  5) * 1 =  0 *  1 = 0
   5/ 3 (~  1) [mod 10]
      ( 5* 3 -  1* 5) *  1 = ( 5 -  5) * 1 =  0 *  1 = 0
   5/ 7 (~  1) [mod 10]
      ( 5* 7 -  1* 5) *  1 = ( 5 -  5) * 1 =  0 *  1 = 0
   5/ 1 (~  1) [mod 10]
      ( 5* 1 -  1* 5) *  1 = ( 5 -  5) * 1 =  0 *  1 = 0
****2/1****
   8/ 9 (~  1) [mod 10]
      ( 2* 9 -  1* 8) *  1 = ( 8 -  8) * 1 =  0 *  1 = 0
   6/ 3 (~  1) [mod 10]
      ( 2* 3 -  1* 6) *  1 = ( 6 -  6) * 1 =  0 *  1 = 0
   4/ 7 (~  1) [mod 10]
      ( 2* 7 -  1* 4) *  1 = ( 4 -  4) * 1 =  0 *  1 = 0
   2/ 1 (~  1) [mod 10]
      ( 2* 1 -  1* 2) *  1 = ( 2 -  2) * 1 =  0 *  1 = 0
****7/1****
   3/ 9 (~  1) [mod 10]
      ( 7* 9 -  1* 3) *  1 = ( 3 -  3) * 1 =  0 *  1 = 0
   1/ 3 (~  1) [mod 10]
      ( 7* 3 -  1* 1) *  1 = ( 1 -  1) * 1 =  0 *  1 = 0
   7/ 1 (~  1) [mod 10]
      ( 7* 1 -  1* 7) *  1 = ( 7 -  7) * 1 =  0 *  1 = 0
   9/ 7 (~  1) [mod 10]
      ( 7* 7 -  1* 9) *  1 = ( 9 -  9) * 1 =  0 *  1 = 0
****9/1****
   7/ 3 (~  1) [mod 10]
      ( 9* 3 -  1* 7) *  1 = ( 7 -  7) * 1 =  0 *  1 = 0
   9/ 1 (~  1) [mod 10]
      ( 9* 1 -  1* 9) *  1 = ( 9 -  9) * 1 =  0 *  1 = 0
   1/ 9 (~  1) [mod 10]
      ( 9* 9 -  1* 1) *  1 = ( 1 -  1) * 1 =  0 *  1 = 0
   3/ 7 (~  1) [mod 10]
      ( 9* 7 -  1* 3) *  1 = ( 3 -  3) * 1 =  0 *  1 = 0
****4/1****
   2/ 3 (~  1) [mod 10]
      ( 4* 3 -  1* 2) *  1 = ( 2 -  2) * 1 =  0 *  1 = 0
   6/ 9 (~  1) [mod 10]
      ( 4* 9 -  1* 6) *  1 = ( 6 -  6) * 1 =  0 *  1 = 0
   4/ 1 (~  1) [mod 10]
      ( 4* 1 -  1* 4) *  1 = ( 4 -  4) * 1 =  0 *  1 = 0
   8/ 7 (~  1) [mod 10]
      ( 4* 7 -  1* 8) *  1 = ( 8 -  8) * 1 =  0 *  1 = 0
****0/1****
   0/ 1 (~  1) [mod 10]
      ( 0* 1 -  1* 0) *  1 = ( 0 -  0) * 1 =  0 *  1 = 0
   0/ 9 (~  1) [mod 10]
      ( 0* 9 -  1* 0) *  1 = ( 0 -  0) * 1 =  0 *  1 = 0
   0/ 3 (~  1) [mod 10]
      ( 0* 3 -  1* 0) *  1 = ( 0 -  0) * 1 =  0 *  1 = 0
   0/ 7 (~  1) [mod 10]
      ( 0* 7 -  1* 0) *  1 = ( 0 -  0) * 1 =  0 *  1 = 0
****6/1****
   2/ 7 (~  1) [mod 10]
      ( 6* 7 -  1* 2) *  1 = ( 2 -  2) * 1 =  0 *  1 = 0
   8/ 3 (~  1) [mod 10]
      ( 6* 3 -  1* 8) *  1 = ( 8 -  8) * 1 =  0 *  1 = 0
   4/ 9 (~  1) [mod 10]
      ( 6* 9 -  1* 4) *  1 = ( 4 -  4) * 1 =  0 *  1 = 0
   6/ 1 (~  1) [mod 10]
      ( 6* 1 -  1* 6) *  1 = ( 6 -  6) * 1 =  0 *  1 = 0
****1/1****
   7/ 7 (~  1) [mod 10]
      ( 1* 7 -  1* 7) *  1 = ( 7 -  7) * 1 =  0 *  1 = 0
   9/ 9 (~  1) [mod 10]
      ( 1* 9 -  1* 9) *  1 = ( 9 -  9) * 1 =  0 *  1 = 0
   3/ 3 (~  1) [mod 10]
      ( 1* 3 -  1* 3) *  1 = ( 3 -  3) * 1 =  0 *  1 = 0
   1/ 1 (~  1) [mod 10]
      ( 1* 1 -  1* 1) *  1 = ( 1 -  1) * 1 =  0 *  1 = 0
****8/1****
   8/ 1 (~  1) [mod 10]
      ( 8* 1 -  1* 8) *  1 = ( 8 -  8) * 1 =  0 *  1 = 0
   6/ 7 (~  1) [mod 10]
      ( 8* 7 -  1* 6) *  1 = ( 6 -  6) * 1 =  0 *  1 = 0
   4/ 3 (~  1) [mod 10]
      ( 8* 3 -  1* 4) *  1 = ( 4 -  4) * 1 =  0 *  1 = 0
   2/ 9 (~  1) [mod 10]
      ( 8* 9 -  1* 2) *  1 = ( 2 -  2) * 1 =  0 *  1 = 0
****3/1****
   1/ 7 (~  1) [mod 10]
      ( 3* 7 -  1* 1) *  1 = ( 1 -  1) * 1 =  0 *  1 = 0
   9/ 3 (~  1) [mod 10]
      ( 3* 3 -  1* 9) *  1 = ( 9 -  9) * 1 =  0 *  1 = 0
   3/ 1 (~  1) [mod 10]
      ( 3* 1 -  1* 3) *  1 = ( 3 -  3) * 1 =  0 *  1 = 0
   7/ 9 (~  1) [mod 10]
      ( 3* 9 -  1* 7) *  1 = ( 7 -  7) * 1 =  0 *  1 = 0
```