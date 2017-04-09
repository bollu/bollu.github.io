+++
Categories = ["gsoc"]
date = "2015-05-17"
draft = false
title = "Gsoc Proposal for Vispy"
+++

### Abstract

The aim of the project is to improve VisPy, a rendering and plotting library for Python. 
This can be divided into three broad sub-task
* Port interesting examples from other graphics libraries like Glumpy to VisPy
* Bring high-level plotting constructs to VisPy.
* Improve performance by implementing batching (called collections)


### Personal Details

* Name: Siddharth Bhat
* Email: siddu.druid@gmail.com
* IRC nick: bollu
* Telephone: +91 9008765043
* Other contact methods: Skype - druidofdclaw
* Country of residence: India
* Timezone: IST (GMT +5:30)
* Primary language: English


### Project Proposal

#### Port Examples

The aim of porting examples is to be able to show a clear differential between the way
VisPy handles OpenGL versus other libraries.

Examples also form a handy way of demonstrating the capabilities of the library and also indirectly
help in code coverage. they tend to be non-trivial and involve large parts of the API, making them a
very helpful testing tool.

Examples that are visually interesting and technically challenging examples from another graphics library - such as [Glumpy](https://glumpy.github.io/) into VisPy.

#### Bring high level plotting constructs

As of now, most of VisPy exposes quite a low level API - It provides an Object Oriented interface to
```OpenGL ES 2.0```. 

To be able to be used for data visualization, VisPy would require support for common graph elements - 
axes, ticks, scaling, labels, etc. Along with this, the API must be sufficiently high level.

We could consider [matplotlib](http://matplotlib.org/) as inspiration for VisPy's API. The ease of use is demonstrated here by using [pyplot, a collection of functions to mimic MATLAB](http://matplotlib.org/1.4.1/users/pyplot_tutorial.html)

As of now, there is a rudimentary api in VisPy called as ```mpl_plot``` that aims to provide a matplotlib like
interface. This will be improved upon. 

#### Implement Collections

Right now, lots of render calls that could be batched in VisPy are not batched due to the
architecture that it uses.

To change this, one will need to implement batching (what VisPy refers to as "collections"). There is an
existing implementation in Glumpy which needs to be ported over to VisPy.

Also, these collections need to interface properly with the OpenGL API, as well as maintain state and
transforms. This will require the collections to be fairly high-level.

For example, the collections API will need to interface with the scene graph to maintain transforms that
are applied to it (parent child relationships would be represented by the scene graph).

Since this effects (_should this be affects?_) almost every drawable in VisPy, it will require a
heavy refactoring of the ```Visuals``` layer of the VisPy codebase 


### Project Timeline / Schedule of Deliverables


#### Week 1 - Week 4 (25 May - June 21)
Begin porting collections from [Glumpy's collections module](https://github.com/glumpy/glumpy/tree/master/glumpy/graphics/collections)

port specific collections: 

[PathCollection](https://github.com/glumpy/glumpy/blob/master/glumpy/graphics/collections/path_collection.py)

[PolygonCollection](https://github.com/glumpy/glumpy/blob/master/glumpy/graphics/collections/polygon_collection.py)

[MarkerCollection](https://github.com/glumpy/glumpy/blob/master/glumpy/graphics/collections/marker_collection.py)

[SegmentCollection](https://github.com/glumpy/glumpy/blob/master/glumpy/graphics/collections/segment_collection.py) 


from glumpy to VisPy


These particular collections have been chosen since they are used in the two large-scale examples
in Glumpy (which is the Graph example and the Tiger example)

#### Week 5 - Week 6(June 22 - July 2) 
##### Can start submitting mid-term evaluation from June 26
##### Mid-term evaluations deadline on July 3

Improve test coverage for collections. Get the new API merged into ```vispy/vispy```

Make sure the examples and the collections API work for the mid term evaluation.


#### Week 7 - Week 8 (July 3 - July 9)

Port the two major collections based examples that are present in GlumPy. 

[Graph Example](https://github.com/glumpy/glumpy/blob/master/examples/graph.py)
![Glumpy Graph](https://glumpy.github.io/_images/graph.png)


[Lorenz Attractor Example](https://github.com/glumpy/glumpy/blob/master/examples/lorenz.py)
<img src="http://glumpy.github.io/_static/screenshots/lorenz.png" style="width: 400px;"/>


Port other collections examples from glumpy


Write test cases for present ```collections``` code. Try to get code coverage for collections
to 100%.


Okay, Can I take a slightly lower schedule ~this time of the month? It's my birthday on the tenth of July, and I would like to visit Delhi for a week-ish to meet my friends and my girlfriend :) I hope that's okay!

#### Week 9 - Week 12 (July 10 - August 2)

Once collections are implemented, provide the ability to draw axes, grids and ticks similar to the way glumpy does it.

Port [Lorenz Attractor](https://github.com/glumpy/glumpy/blob/master/examples/lorenz.py) from glumpy to vispy

#### Week 13 - Week 15  (August 3 - August 17)

provide API access to the collections (grid, axes, ticks) from ```mpl_plot```.

Write examples to showcase ```mpl_plot```

#### Week 16 ( August 11 - August 17)
##### expected pencils-down date is August 17
Padding time for unforeseen problems and changes

tidy up the documentation, make sure the new collections are properly documented

write more tests to increase code coverage of collections

#### Week 17 (August 17 - August 21) (stretch week if need be)
##### end deadline is August 21

This week should ideally not be allocated for any work at all.

more examples could be written, and docs could be tightened. However, the work _must_ be done
by __Week 16__

### Open Source Development Experience

I've pushed code to the [PPSSPP project](https://github.com/hrydgard/ppsspp), which is an open source, multi-platform PSP emulator written mostly in C++.

I used to maintain a sublime text plugin called [SublimeBookmarks](https://packagecontrol.io/packages/Sublime%20Bookmarks)

Even though I have not pushed code to Rust project, I am active on the IRC channel, and have posted [bug reports](https://github.com/rust-lang/rust/issues?q=is%3Aissue+author%3Abollu+is%3Aclosed).

I have tried to contribute to a few Rust projects here and there.

There's a pull request to bring [OpenGL ES up to date in Servo](https://github.com/servo/rust-opengles/pull/85)

I'm also a member of the [Piston Developers](https://github.com/PistonDevelopers), a group of people who are 	trying to build a backend-agnostic game engine in Rust.



### Work/Internship Experience

I interned at IIIT-Hyderabad during the summer of 2014. We were building a flexible provisioning system to host 
[Virtual Labs](#). The codebase was mostly in python, and is available here on [Github](https://github.com/vlead). The codebase is written in Python, and interfaces with OpenVZ to spin up containers for virtualization.  


### Academic Experience

I'm pursuing my bachelor's in Computer Science at the Manipal Institute of Technology. I'm studying in my second semester right now.

### Why Me

I've worked on graphics programming in the past (OpenGL specifically). I'm aware of the math involved - Linear Algebra, 
Matrices, Projective Geometry, Quaternions are some of the most commonly employed mathematical tools in graphics programming.

I've written a lot of python code - I maintain a plugin for Sublime Text that's written in Python.

My summer internship at IIIT-Hyderabad also involved writing (and teaching) (do I add this? how do I prove it?) Python code.

I believe I'm experienced in the three things this project requires - OpenGL, Python and Computational Geometry. I've started
contributing    

### Why Python and VisPy

I've used quite a bit of python in the past and the design and principles of the language really resonate with my ideas as a programmer.
Python is, above all, pragmatic which is something that makes me love the language a whole lot.

When it comes to why VisPy specifically, I've been into gamedev and rendering for a fairly long time. I've always wanted a way to
access openGL in python without losing higher level constructs - this is precisely what VisPy aims to offer. That' why I wish to work with VisPy as a student over this summer 
