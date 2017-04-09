+++
Categories = ["gsoc"]
Description = ""
Tags = []
date = "2015-08-24T21:59:38+05:30"
title = "GSoC VisPy - Update #3"

+++

Here's the next update, slightly delayed because of college.


## SceneGraph is merged

The massive [SceneGraph PR](https://github.com/vispy/vispy/pull/1021) that was open all this while has been merged into vispy master, bringing closure to that beast. That's
part 1 of my GSoC project officially complete!


All of the other changes by me reference a [single pull request](https://github.com/vispy/vispy/pull/1038).

## Borders

The `colorbar` had code to render pixel-width borders, that needed to be adapted for a generic case So, the `ColorBarVisual`
was split again, yielding a `BorderVisual` that's used to draw borders in other parts of VisPy.

## Text Positioning

This was the most challenging part of the past two weeks. The ColorBar needed to have text that was placed correctly, independent of
orientation or transforms associated with it.

I wrote and re-wrote code for this, but I wasn't able to hit on the right solution for close to two days. I talked to Eric, my mentor, who
suggested simplifying the code to handle the most simple case and build it up for there. That worked beautifully, to my astonishment.
Simplifying it let me see the flaw in what I was doing, and incorporating that was a really simple job once that was done.

## Colorbar as a Widget

Until now, the colorbar was sitting in the lower levels of VisPy as a `ColorBarVisual`. Bringing it up to a higher abstraction level required
writing a `Widget` for the colorbar, which came with its own set of interesting problems.

The API is now nice and easy to use, since the colorbar tries hard to auto position itself. It figures out position and dimensions.
It's as easy as setting a colormap and having things work, which feels nice to use.

## `vispy.plot` - ColorBar

The `vispy.plot` module uses the `ColorBarWidget` to provide colorbars for plots. As of now, it's simplistic, since it only places
the color bars in different orientations. The next iteration should have a few more features up its sleeve - automatically figuring out
the data limits for one.

## Widget Placement

The plan for next week is to implement the [Cassowary constraint algorithm](https://constraints.cs.washington.edu/cassowary/) for Widget placement. It should be fun, because I don't
know much of linear optimization. The Cassowary algorithm is a derivative of the [simplex algorithm](https://en.wikipedia.org/wiki/Simplex_algorithm), so that looks like a good place
to start when it comes to implementation.

## Odds and Ends - Viridis

Viridis is a colormap that was created as an alternative to `jet`. [There's a PR](https://github.com/vispy/vispy/pull/1044) that
implements it in VisPy.