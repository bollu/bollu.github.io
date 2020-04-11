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


How to check if horizontal scrollbar exists:
```
if ((window.innerHeight - document.documentElement.clientHeight) >0) {
    console.log('H-scrollbar active')
}
```

# TODO
- [x] add warnings for multi-line bold and italic.
- [x] newlines
