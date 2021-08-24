# Builder

- Rewrite using [immer](https://github.com/arximboldi/immer) for functional data structures.
- Consider using `git blame` to date articles by time of creation and last updated.
- Add the shit tons of latex shortcuts I want.
- Make list requirements more flexible. allowed `-aa\n\n-bb`
- Builds HTML from the markdown in the blog.
- Splits large markdown file into multiple HTML files.
- Key-value database: [`kissdb`](https://github.com/adamierymenko/kissdb)
- [Unpoly](https://unpoly.com/up.protocol) for fast page prefetching and 
  Navigation inside a static site.
- [`httpserver.h`](https://github.com/jeremycw/httpserver.h) for a minimal HTTP server to run `libCURL` on.
- [`libCURL` API](https://curl.haxx.se/libcurl/c/) to check that all links exist.
- [crankJS](https://crank.js.org/): Lightweight React replacement if I ever need it.
- [Ace editor](https://ace.c9.io/#nav=embedding) for a lightweight text editor inside the browser.
- Can use [bear](https://github.com/rizsotto/Bear) + makefiles instead of being
  forced into using `CMake` just to generate a compilation database.
- TODO: add spaces between newlines.


How to check if horizontal scrollbar exists:
```js
if ((window.innerHeight - document.documentElement.clientHeight) >0) {
    console.log('H-scrollbar active')
}
```

# Features

- [ ] Client side search with [elasticlunr.js](https://github.com/weixsong/elasticlunr.js/)
- [ ] Mimic link structure as in [`viewsourcecode.org`](https://viewsourcecode.org/snaptoken/kilo/08.appendices.html#)
- [ ] Add "go back" link.
- [ ] Add "subscribe" link.
- [x] [utterances](https://utteranc.es/) for comments on the blog.
- [x] error for `_ _`,  `* *`, `__ __`, `** **` that span multiple lines.
- [x] error for inline code blocks that span multiple lines.
- [x] error for incorrectly written LaTeX.
- [x] error for incorrectly formatted lists.
- [x] generate links for headers as github does.
- [x] [Duktape](https://duktape.org/) for JS interpretation for KaTeX?
- [x] MathML
- [x] RSS feed.
- [ ] pull better span-based error messages from lz into here.
- [ ] Broken URL and image link warnings.
- [ ] Disqus / [`gitment`](https://github.com/imsun/gitment) for comments. (I use [utterances](https://utteranc.es/), so I don't need these)
- [ ] Figure out which DOM element causes window to overflow horizontally.
- [ ] [twtxt](https://twtxt.readthedocs.io/en/latest/)
- [ ] Link generation for repeats: eg. "#a\n#a" should have links `a-1` and `a-2`.
- [ ] [SVGBob](https://ivanceras.github.io/svgbob-editor/) for my dinky ASCII art.
