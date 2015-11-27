+++
Categories = ["math", "research"]
Description = ""
Tags = ["math", "research"]
date = "2015-09-17T12:44:32+05:30"
title = "Getting into Sage - a practical guide"

+++

I've been playing around with group theory and graph theory for a while now, so I thought that it was high-time to install [SAGE](), a Python based environment for math computation.

Unfortunately, the documentation for SAGE is very fragmented, so the experience of getting it up and running wasn't particularly enjoyable. However, once you do get it up and running, it's really slick to use.


It's for this reason that I've decided that this will be a living document consisting of all SAGE usage and information that I find out. I'm doing this both as a cheat sheet for myself, and in the hope that this helps someone else.

This will be a living document, which will be constantly updated by me as I learn more of SAGE. Feel free to e-mail me or comment
at the bottom if I've missed something in the documentation.


## Installation

I preferred to install from source. So, I had to clone the repo and build it from scratch. Word of warning: Sage is pretty heavy, and took around two hours to build on my decently powerful machine.

The [installation steps](http://doc.sagemath.org/html/en/installation/source.html) are well documented on Sage's website.

## Usage

### Groups

SAGE has pretty good support for group theory. It has a ton of groups in-built, along with the ability to construct your own groups.

#### Inbuilt Groups
to access the list of inbuilt groups, type


```
groups.[tab]
```

This has a list of group subsections

```
groups.affine
groups.matrix
groups.misc
groups.permutation
groups.presentation
```

Some groups of interest:

* `groups.affine` contains [affine groups](http://doc.sagemath.org/html/en/reference/groups/sage/groups/affine_gps/affine_group.html) (I don't know much about them)

* `groups.matrix` contains `GL` (invertible matrices), `GU` (unitary group - group of matrices that preserve inner product structure), `GO` (orthogonal group - group of matrices with determinant +-1) and others.

* `groups.permutation` contains the permutation groups.

* `groups.permutation.Alternating(n)` (the alternating group on n letters `A_n`),
*  `groups.permutation.Dihedral(n)` (the dihedral group of order `2n`, `D_2n` which represents the symmetries of a regular polygon of n vertices),
*  `groups.permutation.KleinFour` (the Klein 4 group),
*  `groups.permutation.Symmetric`(the symmetric group of `n` letters `S_n`)

#### Exploring Groups

once you have a group, you can explore its structure in a couple of different ways. One way is to draw the [Cayley table]() of a group like so:

```
sage: grp = groups.permutation.KleinFour()
sage: grp.cayley_table()
*  a b c d
 +--------
a| a b c d
b| b a d c
c| c d a b
d| d c b a
```

#### Constructing Graphs - Direct Product

to take the direct product of two groups, use the `direct_product()` member function

```
grp1 = groups.permutation.cyclic(2)
graph = groups.permutation.

```

#### Constructing Graphs - Semidirect Product (TODO)

#### Constructing Graphs - Restricting the Free Group
One really nice way of constructing groups is to use the [Tietze Representation](https://en.wikipedia.org/wiki/Tietze_transformations) of a group to construct groups.


The idea is to first construct a group with minimal structure with `n` generators, after which one provides relationships in between these generators.

So, to construct the cyclic group of n letters, it has a generator `a`, and the relation `a^n`. The right hand side is automatically assumed to be `= e`.

So, the group defined by that is the group with generator `a` such that `a^n = e`


```
cyclic group of order 3 from free group:

sage: G = FreeGroup('a')
sage: G
Free Group on generators {a}
sage: Cyclic3 = G / [a ** 3]
sage: Cyclic3.structure_description()
'C3'

```


### Graphs

#### Inbuilt graphs
to get a huge selection of graphs, type

```
graphs.[tab]
```

This will open an autocompletion list for a ton of graphs. Some common examples are:

```
graphs.CycleGraph(n) # n is the number of vertices
graphs.CubeGraph(n) # hypercube of n dimensions
graphs.KneserGraph(n, k) # kneser graph with k-size subsets of set n
```



#### Cayley graphs

Another common way to generate graphs is to look at the [Cayley Graph]() of a group. In sage, you can do this by picking a group and calling the `cayley_graph()` method

```
groups.permutation.Cyclic(3).cayley_graph()
```

#### Rendering graphs
To render a graph, use the `show()` method on the graph

```
graphs.CycleGraph(3).show()
```

### GAP


Sage uses [GAP (Groups, Algorithms, Programming)](http://www.gap-system.org/) for Group theory. Unfortunately, it doesn't seem to install all extensions when sage is first installed.

There's an incredibly slick package for GAP knows as [SmallGroups](http://www.gap-system.org/Packages/sgl.html) which lets you characterise a group you construct (as long as the group is in the database).  However, this isn't installed by default which I feel is a real shame.

To install it, run sage as

```
sage -i database_gap
```
Which will pull in and install the package.

To use the package, construct any group and call

```
G.structure_description()
```

which will print out the group that `G` is isomorphic to.
