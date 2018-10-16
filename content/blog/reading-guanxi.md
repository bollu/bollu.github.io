+++
title = "Reading Guanxi"
date = "2018-10-09T11:36:50+05:30"
draft = true

+++

# Motivation

[Guanxi](https://github.com/ekmett/guanxi) is a library by Edward Kmett, which
tries to throw ideas from constraint programming, SAT solving, and propogators
into a blender to see what comes out. I'm quite excited by the project, and
have certain ideas of my own I want to entertain with the framework, so I
decided to perform a deep dive into the codebase.

**I recommend watching
[Edward's twitch.tv streams](https://www.twitch.tv/videos/317285051)
for more a more in-depth view**

I'll be often referring to things I learnt from the stream, since that
seems to be the place most of the "documentation" is happening.


# Project structure

```
.
├── cabal.project
├── guanxi.cabal
├── LICENSE.md
├── questions.txt
├── src
│   ├── Aligned.hs
│   ├── Dyn.hs
│   ├── Env.hs
│   ├── Free.hs
│   ├── Freer.hs
│   ├── Logic
│   │   ├── Class.hs
│   │   ├── CPS.hs
│   │   ├── Naive.hs
│   │   └── Reflection.hs
│   ├── Skew.hs
│   ├── Unaligned.hs
│   ├── Unification.hs
│   └── Unified.hs
└── tags
```

A `tree` reveals:

- The non-existence of a `README` (So this blog will lead to the addition of one!)
- The non-existence of a test suite (Again, I want to add some tests as examples)
- A `questions.txt` file that should be profitable to read.
- `src/Logic/*` which should contain logic programming code
- `src/Unification.hs` which should contain unification theory for
   unifying constraint programming queries.

A quick `git grep prop` shows:

```
git grep prop

src/Env.hs:  act :: a -> Lattice a -> Maybe (Lattice a, Filtered a) -- tells me if it updated and what to pass to propagators
src/Env.hs:    { propagatorAction :: PropagatorId -> s -> m (Set PropagatorId, s)
```

If we want to see the propogators in action, we should probably start
with `Env.hs`.


