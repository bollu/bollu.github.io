+++
date = "2017-06-15T19:57:26+02:00"
title = "GHC newbie part 1: The goal of prettier error messages"
draft = true

+++

# Sales pitch: Better error messages in `GHC` for all

I know, I'm being driven by a goal that maybe lofty. I want colors and pointers in my error messages that tell me where things are going wrong. I want context for these messages. I'd like to engineer an exchange format for GHC consumers for errors. 

With this utopian vision of the future, I set about spelunking the GHC codebase.

My progess at this will be tracked on the revision [D3650](https://phabricator.haskell.org/D3650). I'm writing this in the hopes that it'll inspire others to help out with GHC, and to document my experience with the project.

#### `GHC` can't use `clang` for preprocessing.

#### Where do I add a new library?

I had no idea where to find this information. I randomly fumbled around, copying
files till I read the `packages` file. This turned out the be the correct-ish place to
do this. You also have to update `ghc.mk` for the `STAGE0` and `STAGE1` libraries
so it gets picked up.

##### `packages` cannot deal with `project.cabal`

Turns out that you're not allowed to add multi-packages into `packages`. `prettyprint` contains a `project.cabal` which internally contains the "core" prettyprinter plus some others. 
I spent around half an hour trying to understand what I was doing wrong. I then noticed that it's not a "simple project", but a collection of projects.

Taking an educated guess, I pulled the core out into a [`prettyprinter-core`](https://github.com/bollu/prettyprinter-core) which I've now added into `packages`.
This turned out to be correct, and now both `text` and `prettyprinter-core` can be imported within `GHC`.

##### Next steps: replace uses of `pretty` with `prettyprinter.`


##### As an aside: `text` for both `Cabal` and `prettyprinter`

`hvr` pinged me when I first put up the patch. He told me that `Cabal` would also like to depend on `text` to use
`parsec`. `haddock` would like to have `text` as well. So, I personally believe that this is the "correct" way forward. However, the problem with this is that anyone who
consumes `ghc:lib` will be locked on to the version of `text` that GHC uses. [There's a trac issue asking the number of reverse dependencies `ghc-lib` has](https://ghc.haskell.org/trac/ghc/ticket/13009#comment:5).

I'm on a quest to add `prettyprinter` into GHC for better error messages.

##### Things that caught my eye:

- `llvmCodeGen` is the entry function to the `LLVM` subsytem. Since I eventually want to port ideas from [simplexhc](http://github.com/bollu/simplexhc),
this is probably a good thing to keep in mind `;)`

- There is place where we [set `ribbonsPerLine` to 1.5](https://github.com/ghc/ghc/blame/master/compiler/utils/Pretty.hs#L990). This doesn't make sense,
because it's a normalized value. Also, it's from 15 years ago, which means I was 5 years old back then. Feels surreal that I'm editing code
from back then, to be honest.


##### Random pages that helped a lot
- [The section on `make` and the build system](https://ghc.haskell.org/trac/ghc/wiki/Building/Using)
  The single most helpful thing on that page is: to build `stage2`, run `make 2`.


