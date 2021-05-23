# Euler characteristic for polyhedra, digital geometry, barvinok

## The problem definition

## The hammer: Euler characteristic

## Constructing the euler characteristic

### Bootstrapping from 1D
in 1D, how many types of intervals do we have? 4:
- open-open: `(a, b)`
- open-closed: `(a, b]`
- closed-open: `[a, b)`
- closed-closed: `[a, b]`.


We want to build a function `χ: Intervals -> Z` such that
`χ(interval) = #of vertices - #of edges`. Specifically:

- **closed-closed**: `[a, b]` has one edge between `a` and `b` and two vertices
  at `a` and `b`. So `χ([a, b]) = 2 - 1 = 1`.
- **open-open**: `(a, b)` has one edge between `a` and `b`, and no vertices.
  So `χ((a, b)) = 0 - 1 = -1`
- **closed-open**: `[a, b)` has one edge between `a` and `b` and a single vertex
  at `a`. So `χ([a, b)) = 1 - 1 = 0`.


#### Indicator functions


```
Reals and the interval [a, b]:

----[=====]----
    a     b

f corresponding to [a, b]:

  |
1 |   x=========x
  |    
0 *===o---------o======>
      a         b
```


#### Constructing `χ`:


#### `δ[f]`

```
Reals and the interval [a, b]:

----[=====]----
    a     b

f corresponding to [a, b]:

  |
1 |   x=========x
  |    
0 *===o---------o======>
      a         b

δ[f](x) = lim[ε → 0+] f(x) - f(x-ε)
```

#### `χ` of the closed-closed interval:


```
x < a:

    f(x-ε)=0    f(x)=0
  |   |          |
1 |   |          |    x=========x
  |   |          |     
0 *===|==========|====o---------o======>
      |          |    a         b
     x-ε         x

δ[f](x) = 0 when x < a
```


```
x = a:

    f(a-ε)=0   
      |   f(a)=1
  |   |    |
1 |   |    x=========x
  |   |     
0 *===|====o---------o======>
      |         a         b
     x-ε    

δ[f](a) = lim[ε → 0+]  f(a) - f(a - ε) = 1 - 0 = 1
```

```
Case 3: a < x <= b [notice the <= !]

            f(x-ε) = 1
             |
             |  f(x) = 1
  |          |  |
1 |       x==|==|====x
  |        
0 *=======o---------o======>
          a         b

δ[f](x) = 1 - 1 = 0 when a < x <= b
```

```
Case 4: b < x

            
                        f(x-ε) = 0
1 |        x========x   |   f(x) = 0
  |                     |   |
0 *=======o---------o===|===|==>
          a         b

δ[f](x) = 0 - 0 = 0 when b < x
```

The open-ness at `b` is vital for this to work. Because we are open at `b`, if
you pick some `x > b`, then I can write `x = b + α`, where `α > 0`.
I can then choose
`ε = α/2` to give me:

```
=  f(x) - f(x-ε)
= f(b + α) - f(b + α - α/2)
= f(b + α) - f(b + α/2)    [α/2 > 0 since α > 0]
= 0 - 0 = 0
```
#### `χ` of closed-closed

#### `χ` of open-open

```
### ----(=====)----
        a     b

f2
    
  |             
1 |             o==============o
  |                  
0 *=============x--------------x=========>
                a              b
- δ[f2](x <= a) = 0
- δ[f2](a < x < b) = 0
- δ[f2](x = b) = 0 - 1 = -1
- δ[f2](b < x) = 0

χ(f2) = 0 + 0 + -1 + 0 = -1
```

**What happens at `b`?**

```
                        f2(b-ε) = 1
                         |
                         |     f2(b) = 0
                         |     |
  |                      |     |
1 |             o========|=====o
  |                            |
0 *=============x--------------x=========>
    
δ[f2](x = b) = f2(b) - f2(b-ε) = 0 - 1 = -1
```

In words:

```
f(b) - lim[ε->0+] f(b-ε)
[Let ε = L/2]
~= f(b) - f(b-L/2)
~= f(b) - f(a/2 + b/2) [the point a/2 + b/2 is in the middle of `(a, b)`]
~= 0 [outside] - 1 [inside]
~= -1
```

#### `χ` of open-closed: `(a, b]`:

```
f3:

1 |             o=============x
  |                            
0 *=============x-------------o=========>

- δ[f3](x <= a) = 0
- δ[f3](a < x <= b) = 0
- δ[f3](b < x) = 0

χ(f3) = 0 + 0 + 0 = 0
```

#### `χ` of closed-open


```
f4:

1 |             x=============o
  |                            
0 *=============o-------------x=========>

- δ[f4](x < a) = 0
- δ[f4](x = a) = 1 - 0 =  1
- δ[f4](a < x < b) = 0
- δ[f4](x = b) = 0 - 1 = -1
- δ[f4](b < x) = 0

χ(f4) = 0 + 1 + 0  -1 = 0
```

#### `χ` of unions

#### Review: what we have achieved so far


#### Upgrading `χ` to 2D:

TODO: add image from barvinok

##### Broke

```
δ[f](x) = lim[ε → 0+] f(x) - f(x-ε)
χ(f) = sum up all the jumps as counted by δ across R
χ(f) = Σ_x δ[f](x)
```

##### Woke

```
χ0(∅)   = 0
χ0(_) = 1

δ1[I](x) = lim[ε → 0+] χ0(I∩{x}) - Χ0(I∩{x-ε})
χ1(I) = Σ_x δ1[I](x)
```

- where `I` is the actual _interval_ associated with `f`.
- `χ0` is the "zero dimensional euler characteristic. It returns `1` if it sees a point, and `0` if it sees an empty set
- we replace `f(x)` with `χ0(I∩{x})`. See that it does the same thing:
- `f(x)` returns 1 if `x` is in the interval `I` which corresponds to `f`. 
- `χ0(I∩{x})` returns `1` if `II∩{x}` contans a single point. That is, if `x ∈ I`. Which is the same as what `f` does.

#### Convex, concave and euler characteristic

#### Relationship to connected components:


#### References
- Alexander Barvinok, Integer Points in Polyhedra
- [Alon Amit's answer on Quora about finding connected components](https://www.quora.com/What-are-some-programming-problems-that-look-hard-at-a-first-glance-but-are-actually-easy/answer/Alon-Amit?ch=10&share=aa23bf2f&srid=TLHW)
