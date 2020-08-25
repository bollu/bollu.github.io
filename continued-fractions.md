Fun number theory fact: how to compute the continued fraction expansion of `sqrt(2)`:

Assume we are given `sqrt(2)`, and the only thing we know about it is
that `sqrt(2)^2 = 2`. So, how do we actually compute its value that we 
know and love: `1.4142...`?

We start by using the only thing we know:

```
sqrt(2)^2 = 2
```

and we try to bound it with inequalites

```
1 < sqrt(2)^2 = 2 < 4
sqrt(1) < sqrt(2) < sqrt(4)
1 < sqrt(2) < 2
```

so we know that `sqrt(2) = 1.______` where the `__` is unknown. We've managed
to find its integral part! OK, let's keep going:

```
1 - 1 < sqrt(2) - 1 < 2
0 < sqrt(2) - 1 < 1
```

Now what? Well, we can try to get a number that's larger than `1` by
taking the reciprocal of `sqrt(2) - 1`, and then trying to write down
its expansion. So we are performing the steps:

```
sqrt(2) = 1 + (sqrt(2) - 1)
sqrt(2) = 1 + 1/[1/(sqrt(2) - 1)]
```



```
1/[sqrt(2) - 1]
= [sqrt(2)+1]/[(sqrt(2) - 1)(sqrt(2) + 1)]
= [sqrt(2)+1]/[2-1]
= [sqrt(2)+1]
```

So we can factor the above expression as:

```
sqrt(2) = 1 + (sqrt(2) - 1)
sqrt(2) = 1 + 1/[1/(sqrt(2) - 1)]
sqrt(2) = 1 + 1/[sqrt(2) + 1]
sqrt(2) = 1 + 1/[1 + sqrt(2)]
```

and we're back to square one, because we have a dangling `sqrt(2)` term.
Well, we can just "expand the recursive equation", to get:

```
sqrt(2)
= 1 + 1/[1 + sqrt(2)]
= 1 + 1/[1 + {1  + 1/[1 + sqrt(2)]}]
= 1 + 1/[2 + 1/[1 + sqrt(2)]]
= 1 + 1/[2 + 1/[2 + 1/sqrt(2)]]
= 1 + 1/[2 + 1/[2 + 1/[2 + ...
```

So we've gotten our hands on the continued fraction expansion of `sqrt(2)`.
OK, but how does this actually **help us**? How can I extract the first
decimal digit of the value of `sqrt(2)` from this? Well, here's how. We 
truncate the continued fraction expansion to the first `k` fractions.
Let's call this `tk` for the "kth" truncation. Let's compute some of them:

<<<take table from hatcher>>>

Notice how once some initial string of digits occurs twice in succession,
it remains fixed forever after.
