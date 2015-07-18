+++
Categories = ["editor"]
Description = ""
Tags = []
date = "2015-07-16T04:37:42+05:30"
title = "Websites, Editors, Religion"
+++

First thing's first - I moved this from `bollu.github.io` to `pixel-druid.com`.
I've had this domain for around a year now, but I never got around to do anything with it.
The excuse that I was using was that "there is no space on my EC2 instance", which makes no real sense
when you think about it, since it was a fresh off-the-shelf Ubuntu install. It turns out that `nginx` wrote
out an error log file into `/etc/nginx/error.log` that was a whopping __6.4 GB__. Deleting that single file
solved the no-space-left-to-hide problem.

Next was the HTML, CSS and all the other the shiny aspects of running a website.
Having messed around with [Hugo](http://gohugo.io/), which is a static site generator written in Go,
I decided to use it. I really, really, dig Hugo. It's simple, fast, and not at all like
Jekyll with it's byzantine settings. I'm pleased with it, and it looks pretty as well!

Now that we've completed the Websites part of the title, let's move on to editors.
I've been a Vim person ever since I switched to using Linux. This was a combination of two factors - the peer pressure that I "had to learn vim".
The second (arguably more important) factor was the fact that the _only_ proper C++ development environment I've been able to get
up and running was Vim + [YouCompleteMe](https://github.com/Valloric/YouCompleteMe), a
fantastic plugin by [Valloric](https://github.com/Valloric) (Val Markovic).

Recently, I've taken a liking towards Haskell. Unfortunately, Haskell's state of affairs when it comes to tooling is
pretty terrible. The only stable environment that exists is [haskell-mode](https://github.com/haskell/haskell-mode) for Emacs.

So, I set it up. Color me surprised as all hell, but I _dig_ Emacs. It's slick, generally fast, and is honestly awesome. The fact
that I can browse the filesystem using Dired, use `git` with [Magit](https://github.com/magit/magit)
(which is by the way a saner `git` interface that `git` itself), startup Python REPLs with excellent autocompletion, and
all sorts of other nice features is enjoyable as hell.

I think the major mistake I made previously was to _immediately_ install [Evil mode](http://www.emacswiki.org/emacs/Evil), which is
a Vi emulation layer for Emacs. I guess that insulated me from the "real Emacs" while making it easy to hate, since the two
don't fit perfectly.

I hope I'll stick around with Emacs, since it's this really nice environment to use. In fact, I'm writing this in Emacs.
Shout out to [Chopella sir](http://pascal.iiit.ac.in/~choppell/index.html) who asked me to try out Emacs for the first time!
