+++
title = 'Opaqueness in Coq: A Philosophy'
date = 2018-10-23T17:31:38+05:30
draft = true
tags = []
description = ""

# For twitter cards, see https://github.com/mtn/cocoa-eh-hugo-theme/wiki/Twitter-cards
meta_img = ""

# For hacker news and lobsters builtin links, see github.com/mtn/cocoa-eh-hugo-theme/wiki/Social-Links
hacker_news_id = ""
lobsters_id = ""
+++

After hacking a [bunch] [of] [code] in coq, I've come to conclusion that
the _first thing to do_ after writing a `Definition` is to make it `Opaque`.

This seems to force to write _way_ cleaner code, due to the need to actually
think about what property I want to say about the new definition, instead
of just blindly unfolding it (and losing the point of the abstraction in the
first place).

I'm formalizing `ScalarEvolution` for fun right now, and I'm going to try
this there and see how it goes.
