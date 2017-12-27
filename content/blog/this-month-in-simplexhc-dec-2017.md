+++
title = "This Month in Simplexhc"
date = "2017-12-27T17:44:09+05:30"
draft = true

+++

### Performance Benchmarking
I've been trying to get the performance of sxhc down to that of C. On benchmarks such as ackermann and fibonacci, there
was a non-trivial slowdown of GHC in comparison to plain old C. Debugging this was a really fun experience, [and the full report is here]().
The TL;DR is that the way we generate code today, it is hard to figure out the register allocation problem. In C, LLVM is able
to pass arguments through registers, and so it eliminates the overhead of creating a stack frame almost entirely.
In the case of GHC, it is unable to do so, and is somewhat unclear how this can even be done. Thus, it uses stack frames
which causes a loss of performance.

I want to write asm by hand and bench the "ideal" GHC-style assembly against C assembly, but now that college has started, I 
have much less free time.

From next semester, I have the option to take up "independent studies", where I can choose to work on whatever I want and
recieve credits for it. If any of the people who work on GHC or related things would be willing to contribute some time
to mentor me on this stuff, I would be super glad :) I'd get to work on GHC, and GHC would get some more code check in ;)


### GHC plugin
I've been working on a GHC plugin to convert Core to a style of STG simplexhc accepts. [Plugin repo here](https://github.com/bollu/simplexhc-ghc-plugin).
It doesn't work, in the sense that it does not yet produce the output I want.
Learning how to write a GHC plugin was fun, though.

### Side note - This semseter's honors project: Proving loop transformations in Coq

As my semester project, I'll be reading through CompCert, understanding the codebase, and trying to add simple loop transformations to it.
Eventualy, I want to implement a full polyhedral analysis pass on top of CompCert. For now, I'm strugging to prove things about programs with
two statements.

If anyone has experience with this sort of thing, I would gladly appreciate some pointers regarding proving program properties. Please e-mail me
(`siddu.druid@gmail.com`) if you'd like to help out.

### Wrapping up

Aaaand that's it for 2017. I've made some non-trivial progress. I have a compiler that works, and under certain assumptions beats GHC
(though one can argue the tricks are all cheating). I feel that there are many such micro-optimisations that could help GHC, so I plan
on continuing to work on this.

The next part of what I want to optimise is operations on records. I suspect this will be very interesting, and will bring in problems of
laziness, fusion, etc. I've been reading some excellent papers in this area, so I thought I should list them:

- [Projections for Strictness Analysis](https://dl.acm.org/citation.cfm?id=36604). This is really well written, and provides a clear introduction to
strictness analysis, that allows one to decide if we can un-lazy a value. I personally believe that this is the main bottleneck of compiling lazy programs.

- [Theory & practise of demand analysis in haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/2017/03/demand-jfp-draft.pdf). I don't understand any of this paper on the
first read through. I suspect this will be much harder to digest, and some of the paper is marked with `(???)`, so I believe it is still a draft.

- [A short cut to deforestation](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/deforestation-short-cut.pdf) appears to be a technique to opimise over lists. I have not read the paper yet, but I suspect it will be useful.

- [Defunctionalising push arrays](https://svenssonjoel.github.io/writing/defuncEmb.pdf). I haven't understood the paper, but what I did get is that by applying a transformation known as
[Defunctionalization](https://en.wikipedia.org/wiki/Defunctionalization) on a CPS-style encoding of arrays (push-arrays), they are able to create good fusion properties for a DSL they create, which also plays well with an alternate encoding of arrays (pull-arrays).

- [GHC optimisations](https://wiki.haskell.org/GHC_optimisations) This (sadly outdated?) page should contain a list of all optimisations that GHC performs. I would like to see this
maintained. I'm no GHC expert, but if someone has pointers on how to update the page, I'll gladly do it.
