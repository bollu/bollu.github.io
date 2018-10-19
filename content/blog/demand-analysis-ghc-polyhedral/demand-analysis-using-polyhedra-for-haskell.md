+++
title = "Google mentor summit: Demand analysis for haskell using Polyhedra"
date = "2018-10-16T16:24:13+05:30"
draft = true

+++

# Introduction

Demand analysis is the process of constructing the information of
what output depends on what input in a given Haskell function, which
allows us to convert non-strict operations to strict operations.

Demand analysis is a technique that is used when implementing a compiler
for a lazy language, to ensure which operations can be performed immediately.
This is a crucial optimisation to ensure that we can optimise programs well,
since laziness inhibits crucial optimisations that are available in strict 
languages to be applied. 

# Example 1: Encoding list append

Let's consider the standard version of list append:

```hs
List a = Nil | Cons a (List a)

app Nil ys = ys
app (Cons x xs) ys = Cons x (app xs ys)
```

Let's rewrite this to be STG-style, to only have function applications on
atoms (literals or constants), while forcing all other values to be
`let` floated:


```hs
List a = Nil | Cons a (List a)

app Nil ys = ys
app (Cons x xs) ys = 
    let xs' = app xs  ys in 
    Cons x xs'
```

Next, let's elaborate the implicit `ys`, to consider both cases for `ys`,
to make it a little more explicit:


```hs
List a = Nil | Cons a (List a)

-- app Nil ys = ys
app Nil Nil = Nil
app Nil (Cons y ys) = Cons y ys

-- app (Cons x xs) ys = 
--     let xs' = app xs  ys in 
--     Cons x xs'

app (Cons x xs) Nil = 
    let xs' = app xs Nil in 
    Cons x xs'

app (Cons x xs) (Cons y ys) = 
    let xs' = app xs (Cons y ys) in 
    Cons x xs'
```


This is useful, since we now no longer have an "anonymous intermediate
computation" `app xs ys`, but instead we now have a name for this computation.



```
nil: 0
cons x xs: x -> 1, xs -> 2
```

That is, whenever we see a `nil`, we encode it with a value of `0`.
When we see a `cons x xs`, we annnotate the `x` slot with `1`, and the
`xs` slot with `2`.

```
in1:nil|(cons x xs) in2:nil|(cons y ys) out:nil|(cons z zs)
    0         1  2      3         4 5       6         7 8
```

Let's first write out the dependencies in our nil case:
```
app Nil Nil = Nil
(6) out:nil -> (0)in1:nil
(6) out:nil -> (3)in2:nil

app Nil (Cons y ys) = Cons y ys
(7) out:z -> (4)in2:y
(7) out:zs -> (5)in2:ys
```

# Acknowledgements
Thanks to Edward Kmett helping me flesh this idea out, at the google mentor
summit, and thanks to google for flying me over :)
