+++
date = "2017-10-29 05:27:56+02:00"
title = "This week in simplexhc: July 2 2017"
+++



<iframe src="https://ghbtns.com/github-btn.html?user=bollu&repo=simplexhc&type=star&count=true&size=large" frameborder="0" scrolling="0" width="160px" height="30px"></iframe>


<iframe src="https://ghbtns.com/github-btn.html?user=bollu&repo=simplexhc&type=fork&count=true&size=large" frameborder="0" scrolling="0" width="160px" height="30px"></iframe>


This is going to be a weekly blog post about the status of
[simplexhc](http://github.com/bollu/simplexhc), an STG to LLVM compiler.
Read more about this [in the introduction blog post](https://pixel-druid.com/blog/announcing-simplexhc/).

This has been a **long** time coming. Unfortunately, "next week" turned out to be "after 3 months". To quickly cover ground, I'll first write about stuff I've achieved in the past couple months:

- Moved from Haskell to C++.
- Implemented "fully functional" compiler for a subset of STG.
- Have test cases for common patters which all seem to work `:)`.
- Started looking into integration with GHC.
- Started looking into how to GC, how to implement multi-threading.
- Wrote extentions to compiler-explorer so I can easily show differences online.


##### Move from Haskell to C++

I felt more and more tied down by haskell as the project grew, because I'm used to structuring algorithms with mutability in mind. Especially when it came to something like manipulating IR, I felt like I was wasting my time writing stuff in haskell when a large portion of LLVM's ergonomics are meant to fit C++. 

So, I simply rewrote the codebase in C++ from scratch. 

#### Garbage collection for `simplexhc`

I attended LLVM Dev this year, where I met [Denys Shabalin](https://github.com/scala-native/scala-native), one of the core authors of `scala-native`. He mentioned to me that they have a pretty solid GC implementation, and that it should be quite easy to integrate into my project. 

I'm probably going to do just that for the prototype, benchmark, see how performance goes, and then make decisions as to whether I need to change things in the GC.

I'd much rather steal code, because stealing code is 0 effort for me :) 


#### `compiler-explorer` support
[Here is a live link](http://www.pixel-druid.com:10240/#) to `simplexhc` hooked up on `compiler-explorer`. You can try out examples from the [`tests`](https://github.com/bollu/simplexhc-cpp/tree/master/test) folder of the `simplexhc` repo. To browse the examples, you can also press the "floppy disk / save" option in the top bar of UI that will allow one to load samples.

Note that the syntax closely follows that of [David Luposchainsky (quchen)'s `stgi`](https://github.com/quchen/stgi)

To dump `LLVM IR`, please use the option `--emit-llvm` in the options bar.

- [Raw assembly](http://www.pixel-druid.com:10240/#z:OYLghAFBqd5QCxAYwPYBMCmBRdBLAF1QCcAaPECAKxAEZSBnVAV2OUxAHIBSAJgGYA1ACUAqgDkQgvgFYANqgCG6AMp4AtgAc5mAB4Jk03jIaDBAWnNVC0gOwBhQQDE8O%2BwkzIA1kZPcADACCfEL2ABLY9gDSUvwBwUHoigSKggCSAHYEvAAqAJ7S/AAi6Vm89qgZphAACsQamQSCdQ1ZAJTc/ABC8UkppQT5hSWNFVWCtfXqjR3d8fEARngZ%2BBnAgssEAPq8W2jjnSXcMvYQbRad2AO5Bdy2Xddj1byC/Od3RZ09QYvLq%2BsMADuik0w2kJwgui2QJBUjOF34V0aN3O5ku1yGd2%2BgTMZmQigYmEEUJhoPhqAAZvFcTTHpVqrpBHlUejkU8JgVdLMHh8vvMgksVst1noCMRFMgCI0AOrizSaTDoMHHU5QzZbQFyhXoOEsxEY5kIpFZTH3aRBGn4wnErbqzUg7UTc6U6m0sxs%2BkTRmGtH60aeyHc83BWyfOY/AV/YWCNBaVDWw7g04UuFdVC6RV6q5pjNK0AQHOZo2CQtK3SUUtZkvpou%2B7M1pVY4M0nRNV1uwX/QTARmJlVwUvEkCVp3FwdN9aBuxh7E05aCCkTHsdH6hvkRwKd6OaZhyOQgxTENvFJPwusDU0PWOaeNEiCi8WSmVaxWCUkbLI7Pb095r8MJTcozWQR1EUec%2BwhKtxzNdsYwJIkdz3A8j1HF0LTdAYninc9NHqLJGi9bl5j/bFODaUg5C4GROFIDIuH8GjUC4Bh9EMJhWHYIx%2BFoGiCHosjyK8EAZH8CiuAAFhoujOAY0gmM4GiGBAUS%2BJksjSDgWAkGvVxMDICgIB0nRiBAFiDFIClXAIPSlIgBZ%2BNIQVDzyLgeNIWN1EwLIAHkMjkFy1NILBQLWHQHPwYhPAIPAADdMCUwK9E8ZhrNcmjNkwSjArFDR%2BPIuQ8AWJTIHI1BNGi%2BkuEUlg2A4Wh8qoqSHPkszDAgXBCBILj6EECotF04huraXi8sE4TRKyyTaOaqrGGU0hVIYtoGs4XgmsC%2BSRrU5bSDi4gGDwSoQHEoA)


- [LLVM IR](http://www.pixel-druid.com:10240/#z:OYLghAFBqd5QCxAYwPYBMCmBRdBLAF1QCcAaPECAKxAEZSBnVAV2OUxAHIBSAJgGYA1ACUAqgDkQgvgFYANqgCG6AMp4AtgAc5mAB4Jk03jIaDBAWnNVC0gOwBhQQDE8O%2BwkzIA1kZPcADACCfEL2ABLY9gDSUvwBwUHoigSKggCSAHYEvAAqAJ7S/AAi6Vm89qgZphAACsQamQSCdQ1ZAJTc/ABC8UkppQT5hSWNFVWCtfXqjR3d8fEARngZ%2BBnAgssEAPq8W2jjnSXcMvYQbRad2AO5Bdy2Xddj1byC/Od3RZ09QYvLq%2BsMADuik0w2kJwgui2QJBUjOF34V0aN3O5ku1yGd2%2BgTMZmQigYmEEUJhoPhqAAZvFcTTHpVqrpBHlUejkU8JgVdLMHh8vvMgksVst1noCMRFMgCI0AOrizSaTDoMHHU5QzZbQFyhXoOEsxEY5kIpFZTH3aRBGn4wnErbqzUg7UTc6U6m0sxs%2BkTRmGtH60aeyHc83BWyfOY/AV/YWCNBaVDWw7g04UuFdVC6RV6q5pjNK0AQHOZo2CQtK3SUUtZkvpou%2B7M1pVY4M0nRNV1uwX/QTARmJlVwUvEkCVp3FwdN9aBuxh7E05aCCkTHsdH6hvkRwKd6OaZhyOQgxTENvFJPwusDU0PWOaeNEiCi8WSmVaxWCUkbLI7Pb095r8MJTcozWQR1EUec%2BwhKtxzNdsYwJIkdz3A8j1HF0LTdAYninc9NHqLJGi9bl5j/bFODaUg5C4GROFIDIuH8GjUC4Bh9EMJhWHYIx%2BFoGiCHosjyK8EAZH8CiuAAFhoujOAY0gmM4GiGBAUS%2BJksjSDgWAkGvVxMDICgIB0nRiBAFiDFIClXAIPSlIgBZ%2BNIQVDzyLgeNIWN1EwLIAHkMjkFy1NILBQLWHQHPwYhPAIPAADdMCUwK9E8ZhrNcmjNkwSjArFDR%2BPIuQ8AWJTIHI1BNGi%2BkuEsTB1EIcw9xi9RFJYNgOFofKqKkhz5LMwwIFwQgSC4%2BhBAqLRdOIYa2l4vLBOE0Sssk2juq4RTlNIVSGLaDrOF4LrAvkma1O20g4uIBg8EqEBxKAA%3D%3D)


#### GHC integration
To integrate into GHC, I need to transfer STG over to my compiler and then compile that down to an object file. [Ben gamari](http://bgamari.github.io/) advised me to write a `core -> core` plugin that sneakily writes out my compiler format of STG to the side. 

That way, it's not "truly" integrated into GHC. Rather, it's a side-channel from which I can conveniently convert hakell-proper to STG, because until now, I've been working with only hand-written STG.

#### Benchmarking and performance

I'm going to directly jump to this next, because I believe I have the basics down in the compiler. I haven't started with either, but I'll be very interested to see in which cases `simplexhc` does better than `GHC`.

Do note that these comparisons will be unfair, because GHC takes care of many things that my compiler is not even *aware* of - GC, multithreading, STM, etc. 

#### Closing thoughts

I wrote this to get into the habit of jotting down what I've been doing in general, and to signal that the project is not dead yet :)

I haven't written anything at all about my current instatiation of STG: It's mostly just regular STG with some type information. I'm going to talk about code generation next time.

One thing I'd like to get working is the docker sandbox working so people can run examples on the `compiler-explorer` link.

Please leave [ideas / issues found on the repo](https://github.com/bollu/simplexhc-cpp/issues/new), they're much appreciated!


