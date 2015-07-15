+++
Categories = ["gsoc"]
date = "2015-07-16T02:37:07+05:30"
title = "Gsoc Vispy Week 5"
draft = false
+++

I'm writing this blogpost since we've come close to hitting a milestone in Vispy's progress - the scenegraph changes
are getting merged! So, now, I can move on to phase 2 - building the plotting infrastructure that Vispy needs.


Also, I've moved the blog from using handrolled html/css/js to Hugo + a standard theme. While not as fun, this is definitely
more maintainable, I've got to admit.

Most of the work for the [Scenegraph update](https://github.com/vispy/vispy/pull/928) is done now. It just needs a little bit of spit
and polish to make sure that everything works fine.


The final Visual that was blocking, the `ColorBarVisual` [was ported](https://github.com/campagnola/vispy/pull/11),
with much joy all around.

Along with that, a nice side-effect of the new SceneGraph system meant that
we could implement [per-fragment Phong lighting](https://github.com/campagnola/vispy/pull/10) for
Vispy. There's still a bug lurking in that piece of code that messes up lighting calculations on the edges, but
it works otherwise. I'll be spending some time tracking that down.

The first order of business is to get `ColorBarvisual` onto the high-level `vispy.plot` API. I've started investigating
how the plotting side of Vispy works.

There's also a lot of things that maybe outside the scope of my summer project, but I'd like to continue contributing
to see them through. Automatic palette generation, better theming support for vispy, working on the `IPython` and web backend
parts of Vispy are all things that I want to work on.

Adios, and see everyone next time!
