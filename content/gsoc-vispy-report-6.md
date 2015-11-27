+++
Categories = []
Description = ""
Tags = ["gsoc"]
date = "2015-08-24T23:17:24+05:30"
title = "GSoC VisPy report 6"

+++

This will be the final update for VisPy while I'm officially under GSoC. There's still some work to be done, so there are still going to be updates. 

## Vispy.Plot - merged

The changes to plotting that I'd worked on have been merged successfully! That's a _huge_ chunk of my GSoC work that's been integrated into master.
[Here's the merged pull request](https://github.com/vispy/vispy/issues/1038)

There's a few things left dangling out, (mostly code improvements that need to happen) which is all documented on [this issue](https://github.com/vispy/vispy/issues/1053)

## Grid System

Like I wrote the last time, the Grid system is taking shape. However, there are still a few kinks to be worked out, which caused me to cross over
the deadline :) [Here is the pull request](https://github.com/vispy/vispy/pull/1049)

## Viridis

A small change pulling the Viridis colormap from matplotlib [was added to VisPy](https://github.com/vispy/vispy/pull/1044). The pull request
exposed a pretty interesting design flaw in VisPy - the shader code for the colormaps doesn't actually render to a texture - it creates
conditional branches which causes a sharp performance decrease as the number of control points increase. I'll try and fix this once GSoC ends,
since it seems very focused and doable.

## Odds and Ends

GSoC was really fun and interesting as an experience! I wish I'd set goals slightly more realistically, to account for unexpected events and lost time.
However, I think I did okay in that regard, and I suppose you live and learn :) I sure did learn a lot, and I'd love to take this further.

I'm most definitely still going to contribute to VisPy - seeing the library succeed will be awesome. I have a couple of long-term goals with it,
including porting core parts of the library to C/C++ for better performance. However, that's all very up-in-the-air at the moment. I'll blog about that
as things get more settled.

until then, Adiós!

