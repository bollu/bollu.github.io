+++
title = 'Stuff I Learnt This Year 2018'
date = 2018-11-09T12:08:42+05:30
draft = true
tags = ["tags"]
description = "Desc"

# For twitter cards, see https://github.com/mtn/cocoa-eh-hugo-theme/wiki/Twitter-cards
meta_img = "/images/image.jpg"

# For hacker news and lobsters builtin links, see github.com/mtn/cocoa-eh-hugo-theme/wiki/Social-Links
hacker_news_id = ""
lobsters_id = ""
+++

# Coq

I've been writing a _lot_ of Coq this year. I essentially started
the year knowing no Coq, and am now ending the year being comfortable with it,
and having a couple minor bugfixes into coq, which feels great!

## [Pull requests / issues into Coq](https://github.com/coq/coq/issues?utf8=%E2%9C%93&q=author%3Abollu+)

I began by submitting documentation style PRs. I've gradually begun to read
the source code in bits and pieces. I don't really understand the codebase,
but it's been fun to spelunk.


## [Dependence analysis for a toy language](https://github.com/bollu/dependence-analysis-coq/)


## [SCEV](https://github.com/bollu/scev-coq)

## [PolyIR](https://github.com/bollu/polyir)

# Gauge theory

I took a course on gauge theory. While handwavy, I did get to appreciate
a lot of the ideas, and I at least now know the vocabulary behind this stuff.

I know believe I know what symmetry breaking means at least, and that makes me
happy. 

- [Link to my incomplete, poorly written lecture notes](https://github.com/bollu/subject-notes/blob/master/physics/main.pdf)

# Complexity theory / Algorithms / Crypto

I learnt a good bit of theoretical computer science over the last two
semesters, mostly thanks to good courses. I took courses on complexity theory,
algorithms, and information security.

The information security course was great, not only because I learnt a bunch
of awesome things, but also becase it forced me to learn `LaTeX` properly!

I now have a reasonable view of the basic complexity classes, such as the
space and time hierarchy, interactive complexity classes (such as `IP`, and
more vaguely `ZKP`), and circuit complexity.


- [Infosec lecture notes (does not render well)](https://github.com/bollu/subject-notes/blob/master/pois/main.pdf)
- [Complexity theory lecture notes](https://github.com/bollu/subject-notes/blob/master/complexity/main.pdf)
- [Algorithms lecture notes](https://github.com/bollu/subject-notes/blob/master/caa/main.pdf<Paste>)



# FPGAs

I've been learning how FPGA's work, due to a course in college. I've been
interested in the inherent reconfigurability of FPGAs. The questions I've had
in this space were:

- How easy/hard is it to design hardware for an FPGA?
- Can we create hardware for pure functional languages (aka, STG on hardware)?
- What kind of languages exist for programming FPGAs? Would a haskell-like language
do well?

- TL;DR: It's hard.
- We probably can create STG on hardware, since people have done so before [TODO]
- None of the current languages meet my taste, so I build one.

The current situation as far as I can see is like this:

- Use a language such as System Verilog, which is poorly designed (in the sense
of C), and leaves way too many low level details / sharp corners for my taste

- Use a language such as BlueSpec verilog, which is proprietary (like most things in this space),
and is _still_ not nice enough for me to consider, because frankly the compiler
produces really cryptic error messages, and debugging it is a real pain. I program
in Haskell, I'm not confused by the purity, or the type strictness of the language.
However, it does take many questionable decisions -- eg, not having an escape
hatch for `IO` to debug, no real introspection possible during the elaboration
portion, etc. But it's by far the nicest thing I've found, which is sad in itself

- Use a DSL embedded into Scala such as Chisel. I haven't tried this one out yet,
but I'm somewhat reluctant, because my experiences with Scala have been unpleasant
so far (`sbt` is slow, the language is... over designed)

- Use `Clash`, a Haskell to FPGA compiler. I dislike this approach, since I think
hacking GHC to produce verilog is a fundamentally strange idea, since I hold
the strong belief that we should _not_ be using turing complete language to
describe anything, much less circuits.

So, I propose my solution: Write a DSL in haskell that leverages all the
nice concepts discovered by haskellers: I'm currently betting on some combination
of applicatives, profunctors, and arrows to provide me with the right abstractions
I need to describe computations on an FPGA in a "natural" style, while still
being _nice to use_.

I think Haskell's arrow syntax does remarkably well with this, and I have
an experiment trying to get this up and running: [`bollu/dataflow`](https://github.com/bollu/dataflow)


# Haskellisms
Learning Coq was a huge boost to doing funky haskell things, such as
playing weird type level games, and getting used to things such as
`ConstraintKinds`, along with general type level programming goodness.

I also read a lot of papers on my to-read pile, including arrows,
update monads, and hyperfunctions. While doing so, I would up adding to the
docs of related packages (such as `ekmett/hyperfunctions`), or writing
expository blog posts of libraries.


## [Arrows](https://github.com/bollu/haskell-koans/blob/master/arrow.hs)

As mentioned in the FPGAs section, arrows were initially created to describe
parsers as a thing that's in-between in terms of power of `Applicative` and
`Monad`. The intuition is that `Arrow`s represent machines / chains of computation /
dataflow.

## [Hyperfunctions](https://github.com/ekmett/hyperfunctions)

I don't really understand these, but they seem to allow one to construct a
`Monad` which provides one the ability to perform diagonalization. I've
added links to the `hyperfunctions` repo that adds the link I wanted to read.

## [Guanxi / Propogators](https://github.com/ekmett/guanxi)
Edward Kmett introduced me to the idea of propogators 
([tech report: the art of the propogator](http://web.mit.edu/~axch/www/art.pdf)),
which is an idea I find very appealing. The general framework appears to allow
one to mix different kinds of "solving strategies" (think constraint solving + ILP
+ ...), into a neat framework. Edward has a bunch of stuff on how to use this
framework effectively, but I'm in it as a nice theory of how to write solvers.

### [Structs](https://github.com/bollu/blog/blob/master/content/blog/reading-kmett-structs.md)
The `Guanxi` repo contained references to `structs`, so I wound up writing a blog
post about it. TL;DR - it allows one to create a universe of mutability within
GHC, and provides a nice interface to do this with template haskell.

Click on the title for the blog post where I explore the library.


## [Deltas: An algebraic theory of patches (WIP)](http://github.com/bollu/deltas)
I've always felt that patches / patching should have an algebraic structure,
and I'd initially dismissed it as "yeah, patches form a group action on files,
that's trivial". 

I decided that I had to implement this "trivial" idea to be sure, and this 
led me down a rabbit hole. There's something really deep going on in the theory
is my feeling, which I've documented on the `README` of the package.


The reason I want such a library is to be able to write compiler passes that 
I can cheaply "speculate" on and unroll. This would be really handy to have
in LLVM, since we do a lot of things speculatively, but there's no nice way
to roll back. This motivation was arrived at after long discussions with a
friend, [Philip Pfaffe](https://github.com/pfaffe).

Storing snapshots of the compiler state is _super_ expensive, so we _need_ some
way to make it cheap if it's used at all. It would be amazing if someone could
devise a theory that tells us how to

- Automatically derive the correct choice of a "delta / diff / patch" for a
given type.

- Use the algebraic structure (ie, a group) to simplify / collapse rewrites.

- Use / exploit laziness to make sure that we don't pay for what we don't need,
in terms of applying patches.

The `deltas` package is a way for me to explore solutions in this
design space.

# The philosophy of science

I've always been bothered by my belief in science with no real justification
to back it up. I decided to patch this hole, and hence went about reading
and studying the philosophy of science. The books I read were:

## Proofs and refutations, Imre Lakatos

This book is excellent --- it's structured as a student teacher dialogue,
as they go through building a mathematical proof, and different viewpoints
one can have as one goes through the process of performing mathematical thinking.
It's a pleasure to read, though I suspect I haven't really digested a lot of the
book, it's one I need to re read.

## The structure of scientific revolutions, Thomas Kuhn

## Popper on falsifiability

Popper was the first one to actually construct the 

## Paul ferdinand, against method

Amazing viewpoint, wherein he argues that science is inherently a 


# Linguistics

I have a bunch of friends who are computational linguists, so I've always
been interested in the field. The general list of things I've wanted to read
up was on:

- Word embeddings, since they somehow seem to capture the structure of language
  --- perhaps conforming the distributional hypothesis.

- Conlangs, since I find them interesting to look at from an information theory
  perspective. I've heard from the linguists (I have no idea how true this is)
  that natural languages look very different from languages that a coding theorist
  would come up with to transmit information. I've always wanted to understand
  why this is, and where the fundamental difference comes from.

For this year, I read some amount of word embeddings literature, particularly,
`Word2Vec`, `GLoVe`, and the `Poincare embeddings` paper. I found them
fascinating, and when I find the time, I plan on experimenting with 
different spaces for word embeddings to go into.


# Bedtime reading

I often pick up textbooks which I can skim through right before I sleep. I find
that this is a remarkably good way to pick up a lay of the land, which is
helpful the next time one wants to learn ideas in detail. This year, I did
so with:

## [Purely functional data structures, Chris okasaki](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf)
This is a book that describes the implementation and analysis of data structures
for immutable languages. The short version of the story is that before this
book, we did not know how to analyze immutable data structures, since our arguments
for amortized analysis depend on the "old" version of the data no longer existing.

To combat this, okasaki shows how one can leverage _laziness_ as a tactic to
build data structures that have good _amortized_ bounds on time complexity.

Later in the book, ideas are shown on how to convert _amortized_ bounds to
worst-case bounds.

It's an amazing book, that develops the theory compactly and yet rigorously.
I got a lot out of the book, as well as some _really weird_ facts such as:

- [skew binary numbers](https://en.wikipedia.org/wiki/Skew_binary_number_system), which is a number
system where there is at most one carry on each addition. This lets us define data structures 
with `O(1)` `cons` operations, since we learn in the book how to convert number systems
into data structures!


## [Tensor geometry, the geometric viewpoint and its uses](https://www.springer.com/in/book/9783540520184)

This is definitely the coolest book I've read all year, purely for the quotes
at the beginning of each chapter and on the cover of the book. It lifts quotes
from ancient texts of gods, demons, and spirituality, and somehow makes
them fit beautifully into the context of tensor geometry. What this says
about tensor geometry I'm unsure, but it does reflect a deep sense of aesthetics
of the authors that I really appreciated.

They have a penchant for clean notation and geometric thinking, which I absolutely
loved as I read through the book. I'm only on something like chapter 3, but it's
been gorgeous so far and I have no reason to suspect this will change.

For example, the chapter on "Notation" begins with:

>Therefore is the name of it called Babel;
>because the Lord did there confound the language
>of all the earth

The chapter on "Affine spaces" (which are vector spaces where one forgets
the origin) has:

>"Let the thought of the dharmas as all one bring you
>to the So in Itself: thus their origin is forgotten
>and nothing is left to make us pit one against another."
> ~Seng-ts'an

And for one last choice, the chapter on dual spaces has:

>"A duality of what is discriminated takes place in
>spite of the fact that object and subject cannot be defined."
> ~ Lankavatara Sutra

Truly a treat.
