+++
title = "STG, explained"
date = "2017-06-22 17:27:22+02:00"
draft = true
+++

# Introduction

You may have heard that GHC compiles quite differently from most compilers, which is true.
It takes "Haskell code", which is then desugared down to a "mini-haskell" called Core. This is then
compiled to an __abstract machine__ called STG. STG stands for the "Spineless, tagless, G-machine". If the name itself
hasn't sold you on how cool it is, then I don't know what will `:)`. 

It is this abstract machine that I'll be focusing on.
I'm not going to focus on how this abstract machine is later compiled down to 
hardware in this blog post.

Understanding how this machine works will allow someone to 
- Understand the compilation model of Haskell.
- Have a fair idea of what the compiled code will look like.
- Debug space leaks.

# References
- [`quchen` (David)'s excellent application [`STGi`](https://github.com/quchen/stgi) is a nice way to understand STG evaluatio
visually.

- [`simplexhc`](http://github.com/bollu/simplexhc), my pet compiler project also features an STG implementation. However, I am considering
moving to `GRIN`. More on this in the next blog post.

- The original '92 paper, [Implementing functional languages on stock hardware](https://www.google.ch/search?q=implementing+functional+languages+on+stock+hardware&oq=implementing+function&aqs=chrome.1.69i57j69i59.2088j0j7&sourceid=chrome&ie=UTF-8) is _the_ place to learn about STG.

