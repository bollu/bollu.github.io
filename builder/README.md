# Builder

- Builds HTML from the markdown in the blog.
- Splits large markdown file into multiple HTML files.
- TODO: RSS feed.
- TODO: Latex to ASCII.
- we create source highlihting using
  [`source-highlight`](https://www.gnu.org/software/src-highlite/).

- [TTH: the TEX to HTML translator](http://hutchinson.belmont.ma.us/tth/)
- [Hevea](http://hevea.inria.fr/) is used to deal with latex.

- https://www.cprogramming.com/tutorial/unicode.html
- key-value database: [`kissdb`](https://github.com/adamierymenko/kissdb)
- [Unpoly](https://unpoly.com/up.protocol) for fast page prefetching and 
  navigation inside a static site.
- [`httpserver.h`](https://github.com/jeremycw/httpserver.h) for a minimal HTTP server to run `libCURL` on.
- [`libCURL` API](https://curl.haxx.se/libcurl/c/) to check that all links exist.


How to check if horizontal scrollbar exists:
```js
if ((window.innerHeight - document.documentElement.clientHeight) >0) {
    console.log('H-scrollbar active')
}
```

# Features

- [x] error for `_ _`,  `* *`, `__ __`, `** **` that span multiple lines.
- [x] error for inline code blocks that span multiple lines.
- [x] error for incorrectly written LaTeX.
- [x] error for incorrectly formatted lists.
- [x] generate links for headers as github does.
- [ ] figure out which DOM element causes window to overflow horizontally.
- [ ] RSS feed.
- [ ] Broken link warnings.
- [ ] Checkboxes.
- [ ] Link generation for repeats: eg. "#a\n#a" should have links `a-1` and `a-2`.
- [ ] MathML
- [ ] [twtxt](https://twtxt.readthedocs.io/en/latest/)
- [ ] [Duktape](https://duktape.org/) for JS interpretation for KaTeX?
