+++
Categories = ["gsoc"]
date = "2015-07-08"
draft = false
title = "Gsoc Update - Weeks 3 & 4"
+++

I'm sitting at home with the flu and a runny nose, so this is as good a time as any to write another blog post!

I managed to get a decent amount of work done over the past two weeks. I think I can start working on plotting from next week, since most of the Scenegraph update I talked about last time is done. What's left is a polish and performance improvements, which will be done iteratively, and (I'm hoping) in parallel with the plotting API.


### Scenegraph Overhaul

I've spent most of my time [porting over a lot of the visuals](https://github.com/campagnola/vispy/pull/2) from the old system to the new rewrite. It was an interesting exercise to learn the new architecture and to figure out how everything fits. Much of it was a direct rewrite, but some of it was challenging. 

I'm stuck on a few things right now, chief among them being [porting lighting of MeshVisuals](https://github.com/campagnola/vispy/pull/10). I suspect that there is a bug in the shader code / normals calculation, but I've not been able to isolate it properly.



### Vispy.js

Vispy also has an experimental webGL backend that's going to be used with IPython. I've been checking it out, and it's a really interesting project.

Vispy uses a custom domain specific "language" (it's not a language in the Turing completeness sense of the word. It's more of a spec / internal representation (IR) kind of like the LLVM IR) that is designed to represent openGL operations, which is knows as GLIR. This provides a really neat way to specify openGL commands in a nicely serialize-able format.

Vispy (both the python version and the JavaScript version) has an object-oriented abstraction of OpenGL called "gloo". Now, [gloo has been implemented in vispy.js](https://github.com/vispy/vispy.js/pull/14). I plan on implementing ```GLIR``` on top of `gloo` in the coming weeks. That should be a fun exercise (both to learn `gloo` properly and to implement it in JavaScript)

### Odds and Ends

The usual "fix what annoys you" is around this time too.

I added an option to the test suite that lets one [test docstrings](https://github.com/vispy/vispy/pull/995). This feature existed before, but required one to manually run the python file. It's just a tad easier with this change

There was also a "bug" in the installer(`setup.py`) that silently failed to install in development mode (`python setup.py develop`) if `setuptools` wasn't present.  I just [added a warning](https://github.com/vispy/vispy/pull/998) so that this would be reported.

 That's it for this time!
