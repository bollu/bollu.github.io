+++
date = "2017-07-02 20:09:46+02:00"
title = "This week in simplexhc: July 2 2017"

+++

## This week in simplexhc: July 2, 2017



<iframe src="https://ghbtns.com/github-btn.html?user=bollu&repo=simplexhc&type=star&count=true&size=large" frameborder="0" scrolling="0" width="160px" height="30px"></iframe>


<iframe src="https://ghbtns.com/github-btn.html?user=bollu&repo=simplexhc&type=fork&count=true&size=large" frameborder="0" scrolling="0" width="160px" height="30px"></iframe>


This is going to be a weekly blog post about the status of
[simplexhc](http://github.com/bollu/simplexhc), an STG to LLVM compiler.
Read more about this [in the introduction blog post](https://pixel-druid.com/blog/announcing-simplexhc/)

This week has been mostly about spring-cleaning, and getting things in shape.

#### Integrating `optparse-applicative`

I used to have a shabby parsing system for command line options. This got unweildy,
so I'm now switching to [`optparse-applicative`](https://github.com/pcapriotti/optparse-applicative),
a really neat library for encoding command line options.

#### Moving from `pretty` to `prettyprinter`
`prettyprinter` is the new kid on the block when it comes to pretty-printing
libraries. It's really well written, has a ton of examples to boot, and has
clean documentation.

I switched from `pretty` to `prettyprinter`, because I had shoe-horned `prettyprinter`
like features into `pretty`.

#### Simplifying usage of `llvm-hs-pure`

`llvm-hs-pure` does not support a monadic interface for building up the IR.
For some applications, a monadic API is _way_ easier, and IR building is one
of them, IMO. So, I stole some code from a side project of mine,
[`tiny-optimising-compiler`](https://github.com/bollu/tiny-optimising-compiler) that
offers an IR builder interface. I'll need to extend it to generate LLVM, but this
was something I needed to do in any case for the other project as well.

#### Gathering all binds in the STG program

To create a giant switch case of all possible bindings, I first need to gather these up.
I currently have a [`Builder`](https://github.com/bollu/simplexhc/blob/14f8b037407753f4e8b97b6180f31fbc1cb7f8f8/src/StgLLVMBackend.hs#L88) that maintains this context.  I'm not sure if this is the cleanest way to do this, but I just want some
code to compile at this point that I'm desperate.


Now that I have the infrastructure stuff somewhat out of the way, I hope to
get more "actual" code written this week. Goal for this week: get the simplest
STG program to compile `:)`.
