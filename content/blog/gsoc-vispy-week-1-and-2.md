+++
Categories = ["gsoc"]
date = "2015-06-08"
draft = false
title = "Gsoc Update - Weeks 1 & 2"
+++



The first two weeks of GSoC has gone by, and it's time for another blog post! It's been exciting so far, to work on VisPy and figure out what needs to be done, and how I need to approach it.

A high level overview of these two weeks was basically figuring out where everything should fit in properly, and gently getting everything into shape.

### SceneGraph and Plotting

We're waiting on a [*huge* pull request](https://github.com/vispy/vispy/pull/928) to pass through - this one rewrites the entire ```Scenegraph``` system in VisPy. This is critical for my goal of plotting, so I'll be tasked with helping this one along. Once it pushes through, I can start implementing the plotting part of my project goal.


### Plotting

In the meanwhile, the [discussion about the plotting API](https://github.com/vispy/vispy/issues/918) got very lively, where the different API styles were discussed. While I was inclined to use the Grammar of Graphics style API in the beginning, I can now see that it might be impractical in the real-world, where API usability is sadly trumped by beauty. Therefore, we decided to go with the familiar ```manipulatable``` style API rather than the stylistically different _Grammar of graphics_ style API. However, all is not lost :) The _GoG_ style API can still be implement over the regular API, although I suppose that falls into the cabinet of "future work"

### Vispy performance in Interactive Mode

There used to be a severe performance hit on [running Vispy code in interactive mode](https://github.com/vispy/vispy/issues/945) (```python -i <filename>```).


The issue was caused because the Vispy main loop would sleep on idle to reduce CPU usage when run inside an interpreter. However, it was sleeping for much longer than needed, which caused a signifiant drop in FPS.


The fix was to modify the sleep time to get an ideal compromise between sleep versus rendering, which was fixed [in this pull request](https://github.com/vispy/vispy/pull/946) 


### Colorbar

I've also been implementing a [```ColorBar```](https://github.com/vispy/vispy/pull/979) visual for VisPy, that's useful to show color information to the user (and will be integrated into the plotting API later). While doing this, I found [a bug](https://github.com/vispy/vispy/issues/981) with the ```TextVisual``` system, which resizes text objects that have two or more strings attached to them. It's still open, and we're unsure as to why this is 
happening. I'll be taking a harder look at this next week, as well Eric.

### Vispy, IPython and WebGL

There was also some discussion of the WebGL backend that VisPy has - allowing it to be integrated into IPython notebooks (this is super cool, since it lets you write VisPy code and have it execute in-browser, just like matplotlib does). It's still in its infancy, but I've been thinking of helping around with that a bit, since it seems really awesome to use and toy around with.

### IPython extension

[We had a discussion on](https://github.com/vispy/vispy/issues/977) whether the WebGL backend should automatically load if you're using VisPy in an ```IPython``` notebook so you automatically get embeddable visualizations. The decision was to make it an opt-in feature in the form of an ```IPython``` extension. I've [begun work on this](https://github.com/vispy/vispy/pull/982), and will hopefully be merged by tomorrow or day after. 


This needed a little bit of a code change in the ```vispy.test``` modules as well, since the code for testing had to be upgraded. It needs to now check for the existence of IPython and only then run the test cases. 

Similarly, ```Travis CI```  and ```AppVeyor``` had to be changed so they install IPython in the test machines.

All of these changes combined should lay down a stable foundation to work on Vispy's IPython integration.


Other minor things include cleaning up some code, fixing a few documentation bugs here and there, and little things like that.

The plan for the next week is to help finish off the ```SceneGraph``` and start work on the ```vispy.plot``` library, so we can get a high-level plotting interface.

that's all for this week! Thanks for reading :)
