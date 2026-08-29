// https://spec.commonmark.org/0.29/#preliminaries
// TODO: RSS feed.
// Font to try: Iosevka
#include "duktape/duktape.h"
// #include "utf8.h"
#include <fstream>
#include <iostream>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <sys/stat.h>
#include <sys/types.h>
#include <algorithm>
#include <tuple>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <unordered_map>
#include <utility>
#include <vector>

#undef NDEBUG
#include <assert.h>

#ifndef BLOG_ROOT_FOLDER_TRAILING_SLASH // path configured from cmake.
  #error "expected command line option 'BLOG_ROOT_FOLDER_TRAILING_SLASH'"
#endif
// #if  defined(_WIN32) || defined(WIN32) // my build config on windows
// #define BLOG_ROOT_FOLDER_TRAILING_SLASH "C:\\Users\\bollu\\blog\\"
// #else  // unix
// #define BLOG_ROOT_FOLDER_TRAILING_SLASH "/home/bollu/blog/"
// #endif
// #endif


#define CONFIG_WEBSITE_RSS_DESCRIPTION "A universe of Sorts"
#define CONFIG_WEBSITE_URL_NO_TRAILING_SLASH "https://bollu.github.io"

#define CONFIG_INPUT_MARKDOWN_PATH  (BLOG_ROOT_FOLDER_TRAILING_SLASH "/README.txt")
#define CONFIG_KATEX_PATH  (BLOG_ROOT_FOLDER_TRAILING_SLASH "/katex/katex.min.js")
#define CONFIG_PRISM_PATH (BLOG_ROOT_FOLDER_TRAILING_SLASH "/prism/prism.js")
#define OUTPUT_ROOT_DIR_TRAILING_SLASH BLOG_ROOT_FOLDER_TRAILING_SLASH "/out/"
#define OUTPUT_ARTICLES_URL_TRAILING_SLASH "/articles/"
#define OUTPUT_ARTICLES_DIR_TRAILING_SLASH OUTPUT_ROOT_DIR_TRAILING_SLASH OUTPUT_ARTICLES_URL_TRAILING_SLASH


static const int LONG_LATEX_BLOCK_SIZE = 30;
static const int LONG_CODE_BLOCK_SIZE = 60;

// TODO: make this incremental, allow graceful failure
// TOOD: if an article does not compile, convert to plaintext.

#define GIVE
#define TAKE
#define KEEP

using namespace std;

using ll = long long;
static const ll MAX_CHARS = 1e9;




ll hashstr(const char *s, const ll len) {
  const ll p = 53;
  // closest prime below 2^62. found using b(2^62) on
  // https://www.alpertron.com.ar/ECM.HTM
  const ll mod = 1e9 + 9;
  ll h = 0;
  ll ppow = 1;
  for (int i = 0; i < len; ++i) {
    assert(s[i] != '\0');
    h += ((s[i] + 1) * ppow) % mod;
    ppow = (ppow * p) % mod;
  }
  return h;
}


// L for location
struct L {
  ll si, line, col;
  L(ll si, ll line, ll col) : si(si), line(line), col(col){};
  L nextline() const { return L(si + 1, line + 1, 1); }
  L next(char c) const {
    if (c == '\n') {
      return nextline();
    } else {
      return nextcol();
    }
  }

  L next(const char *s) const {
    L l = *this;
    for (int i = 0; s[i] != 0; ++i) {
      l = l.next(s[i]);
    }
    return l;
  }

  L prev(char c) const {
    if (c == '\n') {
      assert(false && "don't know how to walk back newline");
    } else {
      return prevcol();
    }
  }

  L prev(const char *s) const {
    L l = *this;
    for (int i = strlen(s) - 1; i >= 0; --i) {
      l = l.prev(s[i]);
    }
    return l;
  }

  bool operator==(const L &other) const {
    return si == other.si && line == other.line && col == other.col;
  }

  bool operator!=(const L &other) const { return !(*this == other); }

private:
  L nextcol() const { return L(si + 1, line, col + 1); }
  L prevcol() const {
    assert(col - 1 >= 1);
    return L(si - 1, line, col - 1);
  }
};
const L LOC_FIRST = L(0, 1, 1);

std::ostream &operator<<(std::ostream &o, const L &l) {
  return o << ":" << l.line << ":" << l.col;
}

// half open [...)
// substr := str[span.begin...span.end-1];
struct Span {
  L begin, end;
  Span(L begin, L end) : begin(begin), end(end) { assert(end.si >= begin.si); };
  ll nchars() const { return end.si - begin.si; }
};

std::ostream &operator<<(std::ostream &o, const Span &s) {
  return o << s.begin << " - " << s.end;
}


void print_span(FILE *f, Span span, const char *data) {
  const L l = span.begin;
  const L m = span.end;
  assert (l.line <= m.line);

  const int len = strlen(data);
  if (l.si >= len) {
    fprintf(f, "\n%4lld>EOF", l.line);
    return;
  }
  fprintf(f, "\nvv..raw file ..vv\n");
  {
    int i = l.si;
    for (; i >= 1 && data[i - 1] != '\n'; i--) {
    }

    fprintf(f, "%4lld>", l.line);
    string squiggle;
    for (; data[i] != '\0' && data[i] != '\n'; ++i) {
      squiggle += i >= l.si && i <= m.si ? '^' : ' ';
      fputc(data[i], f);
    }
    fputc('\n', f);
    fprintf(f, "%4lld>%s\n", l.line, squiggle.c_str());
  }

  if (l.line == m.line)  {
    fprintf(f, "^^..raw file..^^\n");
    fflush(f);
    return;
  }

  assert(l.line < m.line);
  int i = m.si;
  string squiggle = "";
  fprintf(f, "%4lld>", m.line);
  for (; data[i] != '\0' && data[i] != '\n'; ++i) {
      squiggle += i >= l.si && (i <= m.si)? '^' : ' ';
      fputc(data[i], f);
  }
  fputc('\n', f);
  fprintf(f, "%4lld>%s\n", m.line, squiggle.c_str());
  fprintf(f, "^^..raw file..^^\n");
  fflush(f);
}

void print_loc(FILE *f, L l, const char *data) {
  Span s(l, l);
  print_span(f, s, data);
}


void vprintf_err_loc(L loc, const char *raw_input, const char *fmt, va_list args) {
  cerr << "===\n";
  print_loc(stderr, loc, raw_input);
  cout << "\n---\n";
  char *outstr = (char *)malloc(sizeof(char) * 1e5);
  vsprintf(outstr, fmt, args);
  assert(outstr);
  cerr << outstr;
  cerr << "\n===" << std::endl;
  free(outstr);
}

void printf_err_loc(L loc, const char *raw_input, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprintf_err_loc(loc, raw_input, fmt, args);
  va_end(args);
}

void vprintf_err_span(Span span, const char *raw_input, const char *fmt, va_list args) {
  cerr << "===\n";
  print_span(stderr, span, raw_input);
  cout << "\n---\n";
  char *outstr = (char *)malloc(sizeof(char) * 1e5);
  vsprintf(outstr, fmt, args);
  assert(outstr);
  cerr << outstr;
  cerr << "\n===" << std::endl;
  free(outstr);
}

void printf_err_span(Span span, const char *raw_input, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprintf_err_span(span, raw_input, fmt, args);
  va_end(args);
}

// Print the line, while coloring spans with [delim...delim].
void fprintf_line_marking_delimited_same_open_closed(FILE *f, const L l, const char delim, const char *raw_input) {

    int i = l.si;
    for (; i >= 1 && raw_input[i - 1] != '\n'; i--) { }

    fprintf(f, "%4lld>", l.line);
    bool inside = false;
    string squiggle;
    for (; raw_input[i] != '\0' && raw_input[i] != '\n'; ++i) {
      if (raw_input[i] == delim) { inside = !inside; }

      squiggle += (inside || raw_input[i] == delim) ? '^' : ' ';
      fputc(raw_input[i], f);

    }

    fputc('\n', f);
    fprintf(f, "%4lld>%s\n", l.line, squiggle.c_str());

}

void printf_line_marking_delimited_same_open_closed(const L l, const char delim, const char *raw_input) {
  fprintf_line_marking_delimited_same_open_closed(stderr, l, delim, raw_input);
}

// ===== document AST =====
//
// the document is a vector<BlockTm*> (block terms). prose-bearing blocks hold
// one raw source Span per line after phase A (parseBlocks), and one InlineLine
// of InlineTm* (inline terms) per line after phase B (parseInlines).

struct InlineTm {
  enum class Kind {
    Text,   // raw source bytes; also raw inline HTML.
    Code,   // `...`  (span includes the backticks).
    Latex,  // $...$  (span includes the dollars).
    Italic, // *...* / _..._ ; **...** parses as nested Italic.
    Link,   // [text](url)
    Image,  // @img("path", key: value, ...)
  };
  const Kind kind;
  Span span;
  virtual ~InlineTm() {}

protected:
  InlineTm(Kind kind, Span span) : kind(kind), span(span) {}
};

// the inline terms of one source line.
typedef vector<InlineTm *> InlineLine;

struct InlineText : public InlineTm {
  InlineText(Span span) : InlineTm(Kind::Text, span) {}
};

struct InlineCode : public InlineTm {
  InlineCode(Span span) : InlineTm(Kind::Code, span) {}
};

struct InlineLatex : public InlineTm {
  InlineLatex(Span span) : InlineTm(Kind::Latex, span) {}
};

struct InlineItalic : public InlineTm {
  InlineLine items;
  InlineItalic(Span span, InlineLine items)
      : InlineTm(Kind::Italic, span), items(items) {}
};

struct InlineLink : public InlineTm {
  InlineLine text;
  std::string url;
  InlineLink(Span span, InlineLine text, std::string url)
      : InlineTm(Kind::Link, span), text(text), url(url) {}
};

// attributes of an @img(...) directive.
struct ImgAttrs {
  enum class Size { S, M, L };
  enum class Placement { None, Left, Right, MarginLeft, MarginRight };
  Size size = Size::M;
  Placement placement = Placement::None;
  std::string width;   // explicit CSS width; overrides size when nonempty.
  std::string caption; // block figures render this as the <figcaption>.
  std::string alt;     // defaults to caption when empty.
};

struct InlineImage : public InlineTm {
  std::string url;
  ImgAttrs attrs;
  InlineImage(Span span, std::string url, ImgAttrs attrs)
      : InlineTm(Kind::Image, span), url(url), attrs(attrs) {}
};

// one list item: the marker location plus one entry per content line
// (continuation indentation already stripped).
struct ListItemTm {
  L marker;
  vector<Span> line_spans;  // phase A.
  vector<InlineLine> lines; // phase B.
  ListItemTm(L marker) : marker(marker) {}
};

// article metadata from a ```meta fenced block:
// status/created/last-edited key-values.
// Scratch is a working note published with the garage door open (the
// absent-status default). TechnicalNote and Essay mark the curated best:
// a complete technical exposition, or a piece of reflective prose. BigList
// and ILike mark living list documents ("I like" = lists of things the
// author enjoys); both are lifted out of the chronological post list into
// their own homepage section.
enum class MetaStatus { Scratch, TechnicalNote, Essay, BigList, ILike };

struct BlockTm {
  enum class Kind {
    Heading,      // # ...; hnum counts the #s.
    Paragraph,    // consecutive prose lines.
    List,         // - ...
    NumberedList, // 1. 2. 3. ...
    Quote,        // consecutive > lines.
    CodeBlock,    // ``` fence (```abc renders as sheet music).
    LatexBlock,   // $$ ... $$
    Html,         // <script> ... </script>, emitted verbatim.
    Comment,      // <!-- ... -->, emitted as nothing.
    Meta,         // ```meta fence; renders as nothing.
    Figure,       // @img(...) alone on a line: a block figure.
  };
  const Kind kind;
  Span span;
  virtual ~BlockTm() {}

protected:
  BlockTm(Kind kind, Span span) : kind(kind), span(span) {}
};

struct BlockHeading : public BlockTm {
  int hnum;
  Span line_span;  // phase A: the heading text, starting after the #s.
  InlineLine line; // phase B.
  BlockHeading(int hnum, Span line_span)
      : BlockTm(Kind::Heading, line_span), hnum(hnum), line_span(line_span) {}
};

// consecutive prose lines, delimited by blank lines or other blocks.
// wrap = false for runs of raw block-level HTML (e.g. the index preamble),
// which are emitted without the <p> wrapper.
struct BlockParagraph : public BlockTm {
  vector<Span> line_spans;  // phase A.
  vector<InlineLine> lines; // phase B.
  bool wrap = true;
  BlockParagraph(Span span) : BlockTm(Kind::Paragraph, span) {}
};

// a hyphen (Kind::List) or numbered (Kind::NumberedList) list.
struct BlockList : public BlockTm {
  vector<ListItemTm> items;
  BlockList(Kind kind, Span span, vector<ListItemTm> items)
      : BlockTm(kind, span), items(items) {}
};

struct BlockQuote : public BlockTm {
  vector<Span> line_spans;  // phase A.
  vector<InlineLine> lines; // phase B.
  BlockQuote(Span span) : BlockTm(Kind::Quote, span) {}
};

struct BlockCode : public BlockTm {
  std::string langname;
  BlockCode(Span span, std::string langname)
      : BlockTm(Kind::CodeBlock, span), langname(langname) {}
};

// a span-only raw block: $$latex$$ (Kind::LatexBlock), verbatim <script> html
// (Kind::Html), or a comment (Kind::Comment).
struct BlockRaw : public BlockTm {
  BlockRaw(Kind kind, Span span) : BlockTm(kind, span) {}
};

// an @img(...) directive alone on its line. written directly above a
// paragraph/list/heading, a floated figure anchors to that block: it is
// emitted just before it, so the float top-aligns with it.
struct BlockFigure : public BlockTm {
  std::string url;
  ImgAttrs attrs;
  BlockFigure(Span span, std::string url, ImgAttrs attrs)
      : BlockTm(Kind::Figure, span), url(url), attrs(attrs) {}
};

// article metadata from a ```meta fenced block.
// how an article's body is laid out. TwoColumn (the default) flows the
// body below the title/date line in two columns; SingleColumn opts out
// via `layout: single-column`.
enum class LayoutKind { SingleColumn, TwoColumn };

struct BlockMeta : public BlockTm {
  MetaStatus status = MetaStatus::Scratch;
  char created[11] = {0};     // "YYYY-MM-DD", or empty if absent.
  char last_edited[11] = {0}; // "YYYY-MM-DD", or empty if absent.
  LayoutKind layout = LayoutKind::TwoColumn;
  BlockMeta(Span span) : BlockTm(Kind::Meta, span) {}
};


// return true if haystack starts with needle
bool strpeek(const char *haystack, const char *needle) {
  int i = 0;
  while (haystack[i] != '\0' && needle[i] != '\0' && haystack[i] == needle[i]) {
    i++;
  }
  return needle[i] == '\0';
}

// consume UPTO non-whitespace or newline character. raw_input[retval] will be
// non-whitespace or newline
L consumeIntraLineWhitespace(const char *raw_input, L loc) {
  while (1) {
    char c = raw_input[loc.si];
    if (c == ' ' || c == '\t') {
      loc = loc.next(raw_input[loc.si]);
    } else {
      return loc;
    }
  }
}

// consume UPTO non-whitespace character. raw_input[retval] will be
// non-whitespace/ ALSO consumes newlines.
L consumeInterLineWhitespace(const char *raw_input, L loc) {
  while (1) {
    char c = raw_input[loc.si];
    if (c == '\n' || c == ' ' || c == '\t') {
      loc = loc.next(raw_input[loc.si]);
    } else {
      return loc;
    }
  }
}

// consume till we file delim in raw_input.
// This also consumes DELIM.
L strconsume(L l, const char *raw_input, const char *delim, const char *errfmt,
             ...) {
  const L lbegin = l;
  while (raw_input[l.si] != '\0' && !strpeek(raw_input + l.si, delim)) {
    l = l.next(raw_input[l.si]);
  }

  if (raw_input[l.si] == '\0') {
    va_list args;
    va_start(args, errfmt);
    vprintf_err_loc(lbegin, raw_input, errfmt, args);
    va_end(args);
    assert(false && "unable to consume string.");
  } else {
    assert(strpeek(raw_input + l.si, delim));
    l = l.next(delim);
  }
  return l;
}

InlineTm *tryParseLink(const char *s, const ll len, const L opensq);

// scan one argument of an @img(...) directive: raw text up to a top-level
// `,` or `)`, with double quotes protecting their contents. false if the
// line (or file) ends first.
static bool scanImgArg(const char *s, L &lcur, std::string &arg) {
  arg.clear();
  bool in_quotes = false;
  while (1) {
    const char c = s[lcur.si];
    if (c == '\n' || c == '\0') { return false; }
    if (!in_quotes && (c == ',' || c == ')')) { return true; }
    if (c == '"') { in_quotes = !in_quotes; }
    arg.push_back(c);
    lcur = lcur.next(c);
  }
}

static std::string trimSpaces(const std::string &x) {
  const size_t b = x.find_first_not_of(" \t");
  if (b == std::string::npos) { return ""; }
  const size_t e = x.find_last_not_of(" \t");
  return x.substr(b, e - b + 1);
}

static std::string unquote(const std::string &x) {
  const std::string v = trimSpaces(x);
  if (v.size() >= 2 && v.front() == '"' && v.back() == '"') {
    return v.substr(1, v.size() - 2);
  }
  return v;
}

// an @img("path", key: value, ...) directive, typst-style. the first
// positional argument is the path; named arguments are size (s/m/l),
// float (left/right/margin-left/margin-right), width, caption, alt.
// returns nullptr if the directive is malformed (unclosed on its line).
InlineTm *tryParseImg(const char *s, const ll len, const L lat) {
  assert(strpeek(s + lat.si, "@img("));
  L lcur = lat.next("@img(");

  std::string url;
  ImgAttrs attrs;

  while (1) {
    if (s[lcur.si] == ')') { lcur = lcur.next(')'); break; }
    std::string arg;
    if (!scanImgArg(s, lcur, arg)) { return nullptr; } // unclosed directive.
    if (s[lcur.si] == ',') { lcur = lcur.next(','); }

    if (trimSpaces(arg).empty()) { continue; }

    // `key: value` if the text before the first ':' is a bare word.
    const size_t colon = arg.find(':');
    std::string key = colon == std::string::npos ? "" : trimSpaces(arg.substr(0, colon));
    const bool keyed = !key.empty() &&
        key.find_first_not_of("abcdefghijklmnopqrstuvwxyz-") == std::string::npos;
    if (!keyed) {
      if (url.empty()) {
        url = unquote(arg);
      } else {
        printf_err_loc(lat, s, "@img: unexpected extra positional arg: |%s|",
                       arg.c_str());
      }
      continue;
    }

    const std::string val = unquote(arg.substr(colon + 1));
    if (key == "size") {
      if (val == "s") { attrs.size = ImgAttrs::Size::S; }
      else if (val == "m") { attrs.size = ImgAttrs::Size::M; }
      else if (val == "l") { attrs.size = ImgAttrs::Size::L; }
      else { printf_err_loc(lat, s, "@img: size must be s/m/l: |%s|", val.c_str()); }
    } else if (key == "float") {
      if (val == "left") { attrs.placement = ImgAttrs::Placement::Left; }
      else if (val == "right") { attrs.placement = ImgAttrs::Placement::Right; }
      else if (val == "margin-left") { attrs.placement = ImgAttrs::Placement::MarginLeft; }
      else if (val == "margin-right") { attrs.placement = ImgAttrs::Placement::MarginRight; }
      else {
        printf_err_loc(lat, s,
            "@img: float must be left/right/margin-left/margin-right: |%s|",
            val.c_str());
      }
    } else if (key == "width") {
      attrs.width = val;
    } else if (key == "caption") {
      attrs.caption = val;
    } else if (key == "alt") {
      attrs.alt = val;
    } else {
      printf_err_loc(lat, s, "@img: unknown key: |%s|", key.c_str());
    }
  }

  if (url.empty()) {
    printf_err_loc(lat, s, "@img: missing image path");
    return nullptr;
  }
  return new InlineImage(Span(lat, lcur), url, attrs);
}

// one inline term starting at lbegin: a link, `code`, $latex$, *italic*, an
// @img(...), or a run of raw text up to the next special character.
InlineTm *parseInlineFragment(const char *s, const ll len, const L lbegin) {
  assert(lbegin.si < len);

  if (strpeek(s + lbegin.si, "@img(")) {
    InlineTm *img = tryParseImg(s, len, lbegin);
    if (img) { return img; }
    printf_err_loc(lbegin, s, "malformed @img(...); rendering it as text");
  }

  InlineTm *link = nullptr;
  if (s[lbegin.si] == '[' && (link = tryParseLink(s, len, lbegin))) {
    return link;
  }
  if (s[lbegin.si] == '`') {
    L lcur = lbegin.next('`');
    lcur = strconsume(lcur, s, "`", "unclosed inline code block `...`");
    if (lbegin.line != lcur.line) {
      printf_err_span(Span(lbegin, lcur), s,
                "inline code block `...` not allowed to "
                "be on two different lines. "
                "Found on lines: (%d:%d---%d:%d)",
                lbegin.line, lbegin.col, lcur.line, lcur.col);
      assert(false && "inline code block `...` on two different lines.");
    }
    return new InlineCode(Span(lbegin, lcur));
  }
  if (s[lbegin.si] == '$') {
    L lcur = lbegin.next('$');
    lcur = strconsume(lcur, s, "$", "unclosed inline latex block $");
    if (lbegin.line != lcur.line) {
      printf_err_span(Span(lbegin, lcur), s,
                "inline latex block not allowed to be on two different lines.");
      printf_line_marking_delimited_same_open_closed(lbegin, '$', s);
      assert(false && "inline latex block on two different lines.");
    }
    return new InlineLatex(Span(lbegin, lcur));
  }
  if (s[lbegin.si] == '*' || s[lbegin.si] == '_') {
    // **bold** parses as an Italic nested directly inside an Italic.
    const char delim = s[lbegin.si];
    L lcur = lbegin.next(delim);
    InlineLine items;
    while (1) {
      InlineTm *t = parseInlineFragment(s, len, lcur);
      lcur = t->span.end;
      items.push_back(t);
      if (lcur.si == len) {
        printf_err_span(Span(lbegin, lcur), s,
                        "unmatched italic delimiter: |%c|.", delim);
        assert(false && "unmatched italic delimiter");
      }
      if (s[lcur.si] == '\n') {
        printf_err_span(Span(lbegin, lcur), s,
                        "italic emphasis spread across multiple lines!");
        assert(false && "italic spread across multiple lines");
      }
      if (s[lcur.si] == delim) { break; }
    }
    return new InlineItalic(Span(lbegin, lcur.next(delim)), items);
  }
  // raw text: consume up to the next special character (or an @img( opener;
  // a lone '@' stays ordinary text so emails don't split).
  L lcur = lbegin;
  while (1) {
    lcur = lcur.next(s[lcur.si]);
    const char c = s[lcur.si];
    if (c == '*' || c == '[' || c == ']' || c == '$' || c == '`' || c == '_' ||
        c == '\n' || c == '\0') {
      break;
    }
    if (c == '@' && strpeek(s + lcur.si, "@img(")) { break; }
  }
  return new InlineText(Span(lbegin, lcur));
}

// the inline terms of one line, up to (and excluding) the terminating newline.
InlineLine parseInlineLine(const char *s, const ll len, const L lbegin) {
  InlineLine line;
  L lcur = lbegin;
  while (s[lcur.si] != '\n') {
    InlineTm *t = parseInlineFragment(s, len, lcur);
    lcur = t->span.end;
    line.push_back(t);
  }
  return line;
}

// [text](url). returns nullptr if this is not a well-formed single-line link.
InlineTm *tryParseLink(const char *s, const ll len, const L opensq) {
  assert(opensq.si < len);
  assert(s[opensq.si] == '[');
  L lcur = opensq.next('[');

  InlineLine text;
  while (1) {
    InlineTm *t = parseInlineFragment(s, len, lcur);
    lcur = t->span.end;
    text.push_back(t);
    if (s[lcur.si] == ']') { break; }
    // a `[` without an accompanying `]` on the same line: not a link.
    if (s[lcur.si] == '\n') { return nullptr; }
  }

  const L openround = lcur.next(']');
  if (s[openround.si] != '(') { return nullptr; }

  L closeround = openround;
  while (s[closeround.si] != ')' && s[closeround.si] != '\n' &&
         s[closeround.si] != '\0') {
    closeround = closeround.next(s[closeround.si]);
  }
  // an unclosed `(` -- or one closed only on a later line -- is not a link.
  // (scanning across lines here once swallowed a whole article into a URL.)
  if (s[closeround.si] != ')') { return nullptr; }

  return new InlineLink(Span(opensq, closeround.next(')')),
                        text,
                        std::string(s + openround.si + 1, s + closeround.si));
}

// return if s[lbegin...] = <number>"."
// eg.
// 1.
// 2.
// ... 10.
// NOTE: this does NOT check that it is at the beginning of a new line.
bool isNumberedListBegin(const char *s, const ll len, const L lbegin) {
  L l = lbegin;
  while (l.si < len && isdigit(s[l.si])) {
    l = l.next(s[l.si]);
  }
  // we made progress, didn't hit EOF, and have a "."
  return l.si > lbegin.si && l.si < len && s[l.si] == '.';
}

// LIST :=
//  | INLINE-BLOCK "-" LIST
//  | INLINE-BLOCK
// TOP := BLOCK+
//
// INLINE-BLOCK :=
//  INLINE-LINE | INLINE-LINE *
//
// INLINE-LINE :=
//    | "`" RAWTILL-NO-NEWLINE("`")
//    | "_" INLINE-LINE "_"
//    | "*" INLINE-LINE "*"
//    | "$" RAWTILL-NO-NEWLINE("$")
//
// QUOTE :=
//  | INLINE-LINE ">" QUOTE
//  | INLINE-LINE
//
// BLOCK :=
//   | "$$" RAWTILL("$$")
//   | "<script" RAWTILL("/script>")
//   | "<!--" RAWTILL ("-->")
//   | "```" RAWTILL("```")
//   | "#" LINE | "##" INLINE-LINE | ... | "#####" INLINE-LINE
//   | "-" LIST
//   | ">" QUOTE

// TODO: convert \vert into |
// TODO: preprocess and check that we don't have \t tokens anywhere.

// is `val` a well-formed YYYY-MM-DD date? copies it into out[11] if so.
bool parseMetaDate(const std::string &val, char *out) {
  int y, m, d;
  if (val.size() == 10 && sscanf(val.c_str(), "%4d-%2d-%2d", &y, &m, &d) == 3 &&
      m >= 1 && m <= 12 && d >= 1 && d <= 31) {
    memcpy(out, val.c_str(), 10);
    out[10] = 0;
    return true;
  }
  return false;
}

// parse the contents of a ```meta fenced block into a Meta block term.
// span covers the whole block including fences;
// keys are status/created/last-edited.
BlockMeta *parseMetaBlock(const char *s, const Span span) {
  BlockMeta *meta = new BlockMeta(span);
  const L content_begin = span.begin.next("```").next("meta").next("\n");
  const L content_end = span.end.prev("```");
  std::string content(s + content_begin.si, s + content_end.si);

  size_t linestart = 0;
  while (linestart < content.size()) {
    size_t lineend = content.find('\n', linestart);
    if (lineend == std::string::npos) { lineend = content.size(); }
    std::string line = content.substr(linestart, lineend - linestart);
    linestart = lineend + 1;

    // trim the line; skip blanks.
    const size_t b = line.find_first_not_of(" \t\r");
    if (b == std::string::npos) { continue; }
    const size_t e = line.find_last_not_of(" \t\r");
    line = line.substr(b, e - b + 1);

    const size_t colon = line.find(':');
    if (colon == std::string::npos) {
      printf_err_span(span, s, "meta line has no 'key: value': |%s|", line.c_str());
      continue;
    }
    std::string key = line.substr(0, colon);
    std::string val = line.substr(colon + 1);
    const size_t kb = key.find_last_not_of(" \t");
    key = key.substr(0, kb == std::string::npos ? 0 : kb + 1);
    const size_t vb = val.find_first_not_of(" \t");
    val = vb == std::string::npos ? "" : val.substr(vb);

    if (key == "status") {
      if (val == "technical-note") {
        meta->status = MetaStatus::TechnicalNote;
      } else if (val == "essay") {
        meta->status = MetaStatus::Essay;
      } else if (val == "scratch") {
        meta->status = MetaStatus::Scratch;
      } else if (val == "big-list") {
        meta->status = MetaStatus::BigList;
      } else if (val == "i-like") {
        meta->status = MetaStatus::ILike;
      } else {
        printf_err_span(span, s,
            "meta status must be 'technical-note', 'essay', 'scratch', "
            "'big-list', or 'i-like', got: |%s|", val.c_str());
      }
    } else if (key == "created") {
      if (!parseMetaDate(val, meta->created)) {
        printf_err_span(span, s,
            "meta created must be YYYY-MM-DD, got: |%s|", val.c_str());
      }
    } else if (key == "last-edited") {
      if (!parseMetaDate(val, meta->last_edited)) {
        printf_err_span(span, s,
            "meta last-edited must be YYYY-MM-DD, got: |%s|", val.c_str());
      }
    } else if (key == "layout") {
      if (val == "two-column") {
        meta->layout = LayoutKind::TwoColumn;
      } else if (val == "single-column") {
        meta->layout = LayoutKind::SingleColumn;
      } else {
        printf_err_span(span, s,
            "meta layout must be 'single-column' or 'two-column', got: |%s|",
            val.c_str());
      }
    } else {
      printf_err_span(span, s, "unknown meta key: |%s|", key.c_str());
    }
  }
  return meta;
}

// ===== phase A: block structure =====
//
// the block scanner walks the input a line at a time and produces the
// block-level structure: headings, lists, quotes, raw/fenced blocks, and
// paragraphs. prose-bearing blocks store raw line spans only; their inline
// structure is parsed in a second phase (parseInlines). blank lines separate
// blocks; they are consumed as structure and produce no tokens.

// location of the '\n' terminating the current line.
L findLineEnd(const char *s, const ll len, L l) {
  while (l.si < len && s[l.si] != '\n') { l = l.next(s[l.si]); }
  assert(l.si < len && "input must end in a newline");
  return l;
}

// is the rest of the current line whitespace only?
bool isBlankLine(const char *s, const ll len, L l) {
  while (l.si < len && (s[l.si] == ' ' || s[l.si] == '\t')) {
    l = l.next(s[l.si]);
  }
  return l.si >= len || s[l.si] == '\n';
}

// scan a hyphen or numbered list starting at its first marker. leaves lcur at
// the start of the line following the list.
BlockList *parseListBlock(const char *s, const ll len, L &lcur,
                          const bool numbered) {
  const L lbegin = lcur;
  vector<ListItemTm> items;
  ll curnum = 1;
  L lend = lbegin;
  while (1) {
    ListItemTm item(lcur);
    // consume the marker.
    if (numbered) {
      char marker[24];
      sprintf(marker, "%lld.", curnum);
      if (!strpeek(s + lcur.si, marker)) {
        printf_err_loc(lcur, s,
                       "Expected list item to start with number: |%lld|", curnum);
        assert(false && "list item not respecting numbering.");
      }
      lcur = lcur.next(marker);
      curnum++;
    } else {
      assert(s[lcur.si] == '-');
      lcur = lcur.next('-');
    }
    // consume the item's content lines.
    while (1) {
      const L le = findLineEnd(s, len, lcur);
      item.line_spans.push_back(Span(lcur, le));
      lend = le;
      lcur = le.nextline();
      if (lcur.si >= len || isBlankLine(s, len, lcur)) { break; }
      if (numbered ? isNumberedListBegin(s, len, lcur) : s[lcur.si] == '-') {
        break; // next item.
      }
      if (s[lcur.si] == ' ') {
        // an indented continuation line joins the item, indentation stripped.
        lcur = consumeIntraLineWhitespace(s, lcur);
        continue;
      }
      printf_err_span(Span(item.marker, lcur), s,
                "ERROR: a list item must be followed by an indented "
                "continuation line, a blank line, or the next list marker.");
      assert(false && "incorrectly terminated list item");
    }
    items.push_back(item);
    if (lcur.si >= len || isBlankLine(s, len, lcur)) { break; }
  }
  return new BlockList(numbered ? BlockTm::Kind::NumberedList
                                : BlockTm::Kind::List,
                       Span(lbegin, lend), items);
}

vector<BlockTm *> parseBlocks(const char *s, const ll len) {
  assert(len > 0 && s[len - 1] == '\n' && "input must end in a newline");
  vector<BlockTm *> ts;
  BlockParagraph *para = nullptr; // paragraph currently being accumulated.

  const auto closePara = [&para]() { para = nullptr; };

  L lcur = LOC_FIRST;
  while (lcur.si < len) {
  const L lbegin = lcur;

  // ===blank line: ends the current paragraph, produces nothing===
  if (isBlankLine(s, len, lcur)) {
    closePara();
    lcur = findLineEnd(s, len, lcur).nextline();
    continue;
  }
  if (strpeek(s + lcur.si, "$$")) {
    closePara();
    lcur = lcur.next("$$");

    // TODO: fix error message here, that will get generated from strconsume.
    // I had never thought about the problem that occurs when the opening
    // and closing braces are the same...
    lcur = strconsume(lcur, s, "$$", "unclosed $$ tag.");

    // we need to have $$\n
    if (lcur.si < len && s[lcur.si] != '\n') {
      printf_err_span(Span(lbegin, lcur), s,
                "incorrectly terminated $$."
                "must have newline following.");
      assert(false && "incorrectly terminated $$");
    }

    if (lcur.line - lbegin.line > LONG_LATEX_BLOCK_SIZE) {
      printf_err_span(
          Span(lbegin, lcur), s,
          "WARNING: latex block is very long! Perhaps block is overflowing?");
      assert(false && "very large latex block");
    }
    ts.push_back(new BlockRaw(BlockTm::Kind::LatexBlock, Span(lbegin, lcur)));
    lcur = lcur.nextline();
    continue;
  }
  if (strpeek(s + lcur.si, "<script")) {
    closePara();
    lcur = strconsume(lcur, s, "</script>", "unclosed <script> tag.");
    ts.push_back(new BlockRaw(BlockTm::Kind::Html, Span(lbegin, lcur)));
    continue; // may resume mid-line; the loop re-dispatches from here.
  }
  if (strpeek(s + lcur.si, "<!--")) {
    closePara();
    lcur = strconsume(lcur, s, "-->", "unclosed comment till end of file.");
    ts.push_back(new BlockRaw(BlockTm::Kind::Comment, Span(lbegin, lcur)));
    continue; // may resume mid-line; the loop re-dispatches from here.
  }
  if (strpeek(s + lcur.si, "```")) {
    closePara();
    lcur = lcur.next("```");


    const int LANG_NAME_SIZE = 20;
    char *langname = (char *)calloc(LANG_NAME_SIZE, sizeof(char));
    assert(langname && "unable to allocate memory");
    ll langlen = 0;
    while (s[lcur.si] != '\n' && langlen < LANG_NAME_SIZE - 1) {
      langname[langlen++] = s[lcur.si];
      lcur = lcur.next(s[lcur.si]);
    }
    // error out if the language name is too long.
    if (langlen == LANG_NAME_SIZE - 1) {
      printf_err_span(Span(lbegin, lcur), s,
        "``` has too long a language name: |%s|", langname);
      assert(false && "too long a language name");
    }

    // default language is text.
    if (langlen == 0) {
      strcpy(langname, "");
    }

    assert(s[lcur.si] == '\n');
    lcur = strconsume(lcur, s, "```", "unclosed code block tag.");

    // we need to have ```\n
    if (lcur.si < len && s[lcur.si] != '\n') {
      printf_err_span(Span(lbegin, lcur), s,
                "incorrectly terminated ```."
                "must have newline following ```.");
      assert(false && "incorrectly terminated code block.");
    }

    if (lcur.line - lbegin.line > LONG_CODE_BLOCK_SIZE) {
      printf_err_span(
          Span(lbegin, lcur), s,
          "WARNING: code block is very long! Perhaps block is overflowing?");
      // TODO: convert this to an assert.
      // assert(false && "very large code block");
    }
    if (!strcmp(langname, "meta")) {
      ts.push_back(parseMetaBlock(s, Span(lbegin, lcur)));
    } else {
      ts.push_back(new BlockCode(Span(lbegin, lcur), langname));
    }
    free(langname);
    lcur = lcur.nextline();
    continue;
  }
  if (strpeek(s + lcur.si, "#")) {
    closePara();
    int hnum = 0;
    for (; lcur.si < len && s[lcur.si] == '#'; lcur = lcur.next('#')) {
      hnum++;
    }
    const L le = findLineEnd(s, len, lcur);
    ts.push_back(new BlockHeading(hnum, Span(lcur, le)));
    lcur = le.nextline();
    continue;
  }
  if (s[lcur.si] == '-') {
    closePara();
    ts.push_back(parseListBlock(s, len, lcur, /*numbered=*/false));
    continue;
  }
  if ((lcur.si == 0 || s[lcur.si - 1] == '\n') &&
      isNumberedListBegin(s, len, lcur)) {
    closePara();
    ts.push_back(parseListBlock(s, len, lcur, /*numbered=*/true));
    continue;
  }
  if (s[lcur.si] == '>') {
    closePara();
    BlockQuote *quote = new BlockQuote(Span(lcur, lcur));
    while (1) {
      lcur = lcur.next('>');
      const L le = findLineEnd(s, len, lcur);
      quote->line_spans.push_back(Span(lcur, le));
      quote->span = Span(quote->span.begin, le);
      lcur = le.nextline();
      if (lcur.si >= len || s[lcur.si] != '>') { break; }
    }
    ts.push_back(quote);
    continue;
  }

  // ===@img(...) alone on its line: a block figure===
  if (strpeek(s + lcur.si, "@img(")) {
    L limg = lcur;
    InlineTm *img = tryParseImg(s, len, limg);
    if (img) {
      // block form only when nothing but whitespace follows on the line;
      // otherwise the prose path parses it as an inline image.
      L lend = img->span.end;
      while (s[lend.si] == ' ' || s[lend.si] == '\t') {
        lend = lend.next(s[lend.si]);
      }
      if (s[lend.si] == '\n') {
        closePara();
        const InlineImage *inline_img = (const InlineImage *)img;
        ts.push_back(
            new BlockFigure(img->span, inline_img->url, inline_img->attrs));
        lcur = lend.nextline();
        continue;
      }
    }
  }

  // ===prose line: open or continue a paragraph===
  if (!para) {
    para = new BlockParagraph(Span(lcur, lcur));
    ts.push_back(para);
  }
  const L le = findLineEnd(s, len, lcur);
  para->line_spans.push_back(Span(lcur, le));
  para->span = Span(para->line_spans[0].begin, le);
  lcur = le.nextline();
  }

  return ts;
}

// ===== phase B: inline structure =====
//
// parse the inline structure (emphasis, links, code, latex) inside each
// block's stored line spans, filling the render-ready inline trees.
void parseInlines(vector<BlockTm *> &ts, const char *s, const ll len) {
  for (BlockTm *t : ts) {
    switch (t->kind) {
    case BlockTm::Kind::Heading: {
      BlockHeading *heading = (BlockHeading *)t;
      heading->line = parseInlineLine(s, len, heading->line_span.begin);
      break;
    }
    case BlockTm::Kind::Paragraph: {
      BlockParagraph *para = (BlockParagraph *)t;
      for (const Span &line : para->line_spans) {
        para->lines.push_back(parseInlineLine(s, len, line.begin));
      }
      // a run of raw block-level HTML (e.g. the index preamble's <h1>) is
      // emitted bare: wrapping block-level HTML in <p> is invalid.
      char first_nonws = 0;
      for (const Span &line : para->line_spans) {
        for (ll i = line.begin.si; i < line.end.si && !first_nonws; ++i) {
          if (!isspace(s[i])) { first_nonws = s[i]; }
        }
        if (first_nonws) { break; }
      }
      para->wrap = first_nonws != '<';
      break;
    }
    case BlockTm::Kind::List:
    case BlockTm::Kind::NumberedList: {
      BlockList *list = (BlockList *)t;
      for (ListItemTm &item : list->items) {
        for (const Span &line : item.line_spans) {
          item.lines.push_back(parseInlineLine(s, len, line.begin));
        }
      }
      break;
    }
    case BlockTm::Kind::Quote: {
      BlockQuote *quote = (BlockQuote *)t;
      for (const Span &line : quote->line_spans) {
        quote->lines.push_back(parseInlineLine(s, len, line.begin));
      }
      break;
    }
    default:
      break; // raw blocks carry no inline structure.
    }
  }
}

void vduk_debug_print_stack(duk_context *ctx, const char *fmt, va_list args) {
  char *outstr = (char*)malloc(sizeof(char)*1e5);
  vsprintf(outstr, fmt, args);
  assert(outstr);

  printf("\nvvv%svvv\n", outstr);
  free(outstr);
  printf("[TOP OF STACK]\n");
  const int len = duk_get_top(ctx);
  for (int i = 1; i <= len; ++i) {
    duk_dup(ctx, -i);
    printf("stk[-%2d] = %20s\n", i, duk_to_string(ctx, -1));
    duk_pop(ctx);
  }
  printf("^^^^^^^\n");
}

void duk_debug_print_stack(duk_context *ctx, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vduk_debug_print_stack(ctx, fmt, args);
  va_end(args);
}

// given the raw_input, the span where the raw text of the code is found,
// and the language name, pygmentize it. So give
// ```
// [SPAN BEGIN]abc
// foo[SPAN END]
// ```
// we will pygmentize the string "abc\nfoo"
GIVE char *pygmentize(duk_context *prism_ctx, KEEP const char *raw_input,
                      KEEP const char *lang, const Span span) {

  char *input = (char *)calloc(sizeof(char), span.nchars() + 1);
  assert(input && "unable to allocate memory for pygmentize");
  for (ll i = 0; i < span.nchars(); ++i) {
    input[i] = raw_input[span.begin.si + i];
  }

  // HACK
  if (strlen(lang) == 0 || !strcmp(lang, "text")) {
    return input;
  }

  // we want to run the line:
  // const html =
  //   Prism.highlight(code, Prism.languages.javascript, 'javascript');
  //
  // [Prism(-1)|]

  duk_push_string(prism_ctx, "highlight");
  // [Prism(-2)|"highlight"(-1)]

  duk_push_string(prism_ctx, input);
  // [Prism(-3)|"highlight"(-2)|<input>(-1)]
  //
  duk_get_prop_string(prism_ctx, -3, "languages");
  // [Prism(-4)|"highlight"(-3)|<input>(-2)|Prism.languages(-1)]

  duk_get_prop_string(prism_ctx, -1, lang);
  // [Prism(-5)|highlight(-4)|input(-3)|Prism.languages(-2)|Prism.languages.lang(-1)]

  if (duk_is_undefined(prism_ctx, -1)) {
    printf_err_loc(span.begin, raw_input, "unable to find language in prismJS: |%s|",
              lang);
    assert(false && "unable to support language for syntax highlight");
  }

  duk_swap_top(prism_ctx, -2);
  // [Prism(-5)|highlight(-4)|input(-3)|Prism.languages.lang(-2)|Prism.languages(-1)]
  duk_pop(prism_ctx);
  // [Prism(-4)|highlight(-2)|input(-2)|Prism.languages.lang(-1)]
  duk_push_string(prism_ctx, lang);
  // [Prism(-5)|highlight(-4)|input(-3)|Prism.languages.lang(-2)| "<lang>"(-1)]

  if (duk_pcall_prop(prism_ctx, -5, 3) == DUK_EXEC_SUCCESS) {
    const char *out = duk_to_string(prism_ctx, -1);
    duk_pop(prism_ctx);
    return strdup(out);
  } else {
    printf_err_loc(span.begin, raw_input, "%s", duk_to_string(prism_ctx, -1));
    assert(false && "unable to syntax highlight");
  }
  assert(false && "unable to syntax highlight");
};

enum class LatexType { LatexTypeBlock, LatexTypeInline };

duk_context *load_katex();
std::pair<bool, GIVE char *> compileLatex(duk_context *katex_ctx, KEEP const char *raw_input,
                        const Span span, const LatexType ty) {

  // TODO: fixup inline v/s block math. Code is here:
  // duk_push_obj();
  // duk_set_property(displayMath, true) // or whatever

  // stack:
  // [katex(-1)|]

  char *input = (char *)calloc(span.nchars() + 2, sizeof(char));
  assert(input && "unable t allocate memory for compileLatex");
  for (ll i = 0; i < span.nchars(); ++i) {
    input[i] = raw_input[span.begin.si + i];
  }

  duk_push_string(katex_ctx, "renderToString");
  // [katex(-2)| "renderToString"(-1)]

  duk_push_string(katex_ctx, input);
  // [katex(-3)| "renderToString"(-2)|"<input string>"(-1)]

  duk_push_object(katex_ctx); // { displayMode: ... }
  // [katex(-4)| "renderToString"(-3)|"<input string>"(-2)|Object(-1)]

  duk_push_boolean(katex_ctx, ty == LatexType::LatexTypeBlock);
  // [katex(-5)| "renderToString"(-4)|"<input string>"(-3)|Object(-2)|true/false(-1)]

  const int OPTIONS_IDX = -2;
  duk_bool_t rc = duk_put_prop_string(katex_ctx, OPTIONS_IDX, "displayMode");
  // https://duktape.org/api.html#duk_put_prop
  assert(rc == 1); // returns 1 on success.
  // [katex(-4)| "renderToString"(-3)|"<input
  // string>"(-2)|Object{displayMode:true/false}(-1)] [katex(-4)|
  // renderToString(-3) | raw_str(-2)| displaymode(-1)]

  const int KATEX_IDX = -4;
  if (duk_pcall_prop(katex_ctx, KATEX_IDX, 2) == DUK_EXEC_SUCCESS) {
    // stack: call
    // [katex| out_string]
    //       -2         -1
    char *out = strdup(duk_to_string(katex_ctx, -1));
    duk_pop(katex_ctx);
    return {true, out};
  } else {
    // https://github.com/svaarala/duktape/issues/848
    // [katex| err_string]
    //    -2      -1
    printf_err_span(span, raw_input, "%s", duk_to_string(katex_ctx, -1));
    duk_pop(katex_ctx);
    // reload katex context.
    katex_ctx = load_katex();
    // vim -c "call cursor(19, 11)"
    assert(false && "unable to compile latex span");
    return {false, nullptr};
  }
}

// given the inline object, convert it to text a link can see.
// Ie, for example, on seeing
// - [$A = B$](...) or **what I need**
//
// this will return:
// A = B or what I need
// So this strips off all "decoration" leaving only the text in place.
void inlineToPlaintext(const char *s, const InlineTm *t, char *outs,
                       ll &outlen);

void inlineLineToPlaintext(const char *s, const InlineLine &line, char *outs,
                           ll &outlen) {
  ll len = 0;
  for (const InlineTm *item : line) {
    inlineToPlaintext(s, item, outs + len, len);
  }
  outlen = len + 1;
}

void inlineToPlaintext(const char *s, const InlineTm *t, char *outs,
                       ll &outlen) {
  switch (t->kind) {
  case InlineTm::Kind::Text: {
    strncpy(outs, s + t->span.begin.si, t->span.nchars());
    outlen += t->span.nchars();
    break;
  }
  case InlineTm::Kind::Code: {
    const Span span = Span(t->span.begin.next("`"), t->span.end.prev("`"));
    strncpy(outs, s + span.begin.si, span.nchars());
    outlen += span.nchars();
    break;
  }
  case InlineTm::Kind::Latex: {
    const Span span = Span(t->span.begin.next("$"), t->span.end.prev("$"));
    strncpy(outs, s + span.begin.si, span.nchars());
    outlen += span.nchars();
    break;
  }
  case InlineTm::Kind::Italic:
    inlineLineToPlaintext(s, ((InlineItalic *)t)->items, outs, outlen);
    break;
  case InlineTm::Kind::Link:
    inlineLineToPlaintext(s, ((InlineLink *)t)->text, outs, outlen);
    break;
  case InlineTm::Kind::Image:
    break; // images contribute nothing to plaintext (or slugs).
  }
}

// make a link according to github flavoured markdown convention for
// a heading.
// https://gist.github.com/asabaylus/3071099
GIVE const char *mkHeadingURL(KEEP const char *raw_input,
                              const BlockHeading *heading) {
  const int BUFSIZE = (1 << 10);
  char plaintext[BUFSIZE];
  for (int i = 0; i < BUFSIZE; ++i)
    plaintext[i] = 0;
  ll ptlen = 0;
  inlineLineToPlaintext(raw_input, heading->line, plaintext, ptlen);
  plaintext[ptlen] = 0;
  assert(ptlen + 1 < BUFSIZE && "heading exceeded buffer size limits");

  ll ptbegin = 0;
  while (plaintext[ptbegin] == ' ') {
    ptbegin++;
  }
  ll ptend = strlen(plaintext);
  while (plaintext[ptend] == ' ') {
    ptend--;
  }
  assert(ptend - ptbegin >= 0);

  char *url = (char *)calloc(strlen(OUTPUT_ROOT_DIR_TRAILING_SLASH) + ptlen + 2, sizeof(char));
  assert(url && "unable to allocate memory for making heading URL");

  sprintf(url, "%s", OUTPUT_ARTICLES_URL_TRAILING_SLASH);
  ll url_ix = strlen(url);

  bool seenalnum = true;
  for (ll i = ptbegin; i != ptend; ++i) { // heading index
    // convert uppercase -> lowercase; keep digits; convert space to hyphen;
    // convert groups of hyphens into single hyphen; remove everything else.
    const char c = plaintext[i];
    if ((isalpha(c) || isdigit(c)) && !seenalnum) {
      seenalnum = true;
      url[url_ix++] = '-';
    }

    if (isalpha(c)) {
      url[url_ix++] = tolower(c);
    } else if (isdigit(c)) {
      url[url_ix++] = c;
    } else if (c == '-') {
      // eat repeated hyphens
      while (plaintext[i + 1] == '-') {
        i++;
      }
      seenalnum = false;
    } else if (isspace(c)) {
      seenalnum = false;
    }
  }

  // TODO: strip trailing `-` in URL.
  return url;
}


duk_context *load_katex() {
    FILE *fkatex = fopen(CONFIG_KATEX_PATH, "rb");
    if (fkatex == nullptr) {
      fprintf(stderr, "ERROR: unable to find katex.min.js at |%s|\n",
              CONFIG_KATEX_PATH);
      assert(false && "unable to open katex.min.js");
    }

    fseek(fkatex, 0, SEEK_END);
    const ll len = ftell(fkatex);
    fseek(fkatex, 0, SEEK_SET);
    char *js = (char *)calloc(sizeof(char), len + 10);

    const ll nread = fread(js, 1, len, fkatex);
    assert(nread == len);
    fclose(fkatex);
    duk_context *katex_ctx = duk_create_heap_default();

    duk_push_string(katex_ctx, "katex.min.js");
    // compile katex
    if (duk_pcompile_lstring_filename(katex_ctx, 0, js, len) != 0) {
      fprintf(stderr, "===katex.min.js compliation failed===\n%s\n===\n",
              duk_safe_to_string(katex_ctx, -1));
      assert(false && "unable to compile katex.min.js");
    }

    // run katex to get the global.katex object
    if (duk_pcall(katex_ctx, 0) != 0) {
      fprintf(stderr, "===katex.min.js execution failed===\n%s\n===\n",
              duk_safe_to_string(katex_ctx, -1));
      assert(false && "unable to execute katex.min.js");
    }

    if (duk_peval_string(katex_ctx, "katex") != 0) {
      fprintf(stderr,
              "====katex.min.js: unable to grab katex object===\n%s\n===\n",
              duk_safe_to_string(katex_ctx, -1));
      assert(false && "unable to find the katex object");
    }
    return katex_ctx;
  }

uint32_t murmur_32_scramble(uint32_t k) {
    k *= 0xcc9e2d51;
    k = (k << 15) | (k >> 17);
    k *= 0x1b873593;
    return k;
}

// ===== rendering =====

bool renderInline(duk_context *katex_ctx, duk_context *prism_ctx,
                  const char *raw_input, const InlineTm *t, ll &outlen,
                  char *outs);

bool renderInlineLine(duk_context *katex_ctx, duk_context *prism_ctx,
                      const char *raw_input, const InlineLine &line,
                      ll &outlen, char *outs) {
  bool success = true;
  for (const InlineTm *t : line) {
    success &= renderInline(katex_ctx, prism_ctx, raw_input, t, outlen, outs);
  }
  return success;
}

// minimal escaping for text placed inside single-quoted HTML attributes.
static std::string escapeHtmlAttr(const std::string &x) {
  std::string out;
  for (const char c : x) {
    if (c == '&') { out += "&amp;"; }
    else if (c == '<') { out += "&lt;"; }
    else if (c == '\'') { out += "&#39;"; }
    else { out.push_back(c); }
  }
  return out;
}

// the css classes for a figure's size and float attributes.
static std::string imgFigureClasses(const ImgAttrs &attrs) {
  std::string cls = "fig";
  switch (attrs.size) {
  case ImgAttrs::Size::S: cls += " fig-s"; break;
  case ImgAttrs::Size::M: cls += " fig-m"; break;
  case ImgAttrs::Size::L: cls += " fig-l"; break;
  }
  switch (attrs.placement) {
  case ImgAttrs::Placement::None: break;
  case ImgAttrs::Placement::Left: cls += " fig-left"; break;
  case ImgAttrs::Placement::Right: cls += " fig-right"; break;
  case ImgAttrs::Placement::MarginLeft: cls += " fig-margin-left"; break;
  case ImgAttrs::Placement::MarginRight: cls += " fig-margin-right"; break;
  }
  return cls;
}

bool renderInline(duk_context *katex_ctx, duk_context *prism_ctx,
                  const char *raw_input, const InlineTm *t, ll &outlen,
                  char *outs) {
  switch (t->kind) {
  case InlineTm::Kind::Text: {
    strncpy(outs + outlen, raw_input + t->span.begin.si, t->span.nchars());
    outlen += t->span.nchars();
    outs[outlen] = ' '; outlen++;
    return true;
  }

  case InlineTm::Kind::Code: {
    outlen += sprintf(outs + outlen, "<code class='inline'>");
    const Span span = Span(t->span.begin.next("`"), t->span.end.prev("`"));
    strncpy(outs + outlen, raw_input + span.begin.si, span.nchars());
    outlen += span.nchars();
    outlen += sprintf(outs + outlen, "</code>");
    return true;
  }

  case InlineTm::Kind::Latex: {
    const Span span = Span(t->span.begin.next('$'), t->span.end.prev('$'));
    outlen += sprintf(outs + outlen, "<span class='latexinline'>");

    char *outcompile;
    bool success;
    tie(success, outcompile) =
        compileLatex(katex_ctx, raw_input, span, LatexType::LatexTypeInline);
    if (!success) { return false; }
    strcpy(outs + outlen, outcompile);
    outlen += strlen(outcompile);
    free(outcompile);

    outlen += sprintf(outs + outlen, "</span>");
    return true;
  }

  case InlineTm::Kind::Italic: {
    const InlineItalic *italic = (const InlineItalic *)t;
    outlen += sprintf(outs + outlen, "<i>");
    const bool success = renderInlineLine(katex_ctx, prism_ctx, raw_input,
                                          italic->items, outlen, outs);
    outlen += sprintf(outs + outlen, "</i>");
    return success;
  }

  case InlineTm::Kind::Link: {
    const InlineLink *link = (const InlineLink *)t;
    outlen += sprintf(outs + outlen, "<a href=%s>", link->url.c_str());
    const bool success = renderInlineLine(katex_ctx, prism_ctx, raw_input,
                                          link->text, outlen, outs);
    outlen += sprintf(outs + outlen, "</a>");
    return success;
  }

  case InlineTm::Kind::Image: {
    const InlineImage *img = (const InlineImage *)t;
    const std::string alt =
        img->attrs.alt.empty() ? img->attrs.caption : img->attrs.alt;
    outlen += sprintf(outs + outlen, "<img class='img-inline' src='%s' alt='%s'>",
                      escapeHtmlAttr(img->url).c_str(),
                      escapeHtmlAttr(alt).c_str());
    return true;
  }
  }
  assert(false && "unreachable");
}

const char *mkHeadingURL(const char *raw_input, const BlockHeading *heading);

bool renderBlock(duk_context *katex_ctx, duk_context *prism_ctx,
                 const char *raw_input, const BlockTm *t, ll &outlen,
                 char *outs) {
  switch (t->kind) {
  case BlockTm::Kind::Comment: {
    return true;
  }

  case BlockTm::Kind::Figure: {
    const BlockFigure *fig = (const BlockFigure *)t;
    const std::string alt =
        fig->attrs.alt.empty() ? fig->attrs.caption : fig->attrs.alt;
    std::string style;
    if (!fig->attrs.width.empty()) {
      style = " style='width:" + escapeHtmlAttr(fig->attrs.width) + "'";
    }
    outlen += sprintf(outs + outlen, "<figure class='%s'%s>",
                      imgFigureClasses(fig->attrs).c_str(), style.c_str());
    outlen += sprintf(outs + outlen, "<img src='%s' alt='%s'>",
                      escapeHtmlAttr(fig->url).c_str(),
                      escapeHtmlAttr(alt).c_str());
    if (!fig->attrs.caption.empty()) {
      outlen += sprintf(outs + outlen, "<figcaption>%s</figcaption>",
                        escapeHtmlAttr(fig->attrs.caption).c_str());
    }
    outlen += sprintf(outs + outlen, "</figure>");
    return true;
  }

  case BlockTm::Kind::Meta: {
    // the dates line under the heading; the kicker above the headline
    // carries the status.
    const BlockMeta *meta = (const BlockMeta *)t;
    if (!meta->created[0] && !meta->last_edited[0]) { return true; }

    outlen += sprintf(outs + outlen, "<div class='article-meta'>");
    const char *sep = "";
    if (meta->created[0]) {
      outlen += sprintf(outs + outlen, "created %s", meta->created);
      sep = " · ";
    }
    // showing last-edited only makes sense once it differs from created.
    if (meta->last_edited[0] && strcmp(meta->last_edited, meta->created) != 0) {
      outlen += sprintf(outs + outlen, "%slast edited %s", sep,
                        meta->last_edited);
    }
    outlen += sprintf(outs + outlen, "</div>");
    return true;
  }

  case BlockTm::Kind::Html: {
    strncpy(outs + outlen, raw_input + t->span.begin.si, t->span.nchars());
    outlen += t->span.nchars();
    outs[outlen] = ' '; outlen++;
    return true;
  }

  case BlockTm::Kind::CodeBlock: {
    const BlockCode *block = (const BlockCode *)t;

    // we want to ignore the first 3 ``` and the last 3 ```
    const Span span =
        Span(t->span.begin.next("```").next(block->langname.c_str()).next("\n"),
             t->span.end.prev("```"));

    if (block->langname == "abc") {
      // sheet music, rendered client-side by abcjs.
      outlen += sprintf(outs + outlen, "<div class=\"abc\">");
      strncpy(outs + outlen, raw_input + span.begin.si, span.nchars());
      outlen += span.nchars();
      outlen += sprintf(outs + outlen, "</div>");
      return true;
    }

    // TODO: escape HTML content.
    outlen += sprintf(outs + outlen, "<pre><code>");
    char *code_html =
        pygmentize(prism_ctx, raw_input, block->langname.c_str(), span);
    strcpy(outs + outlen, code_html);
    outlen += strlen(code_html);
    free(code_html);
    outlen += sprintf(outs + outlen, "</code></pre>");
    return true;
  }

  case BlockTm::Kind::LatexBlock: {
    const Span span = Span(t->span.begin.next("$$"), t->span.end.prev("$$"));
    outlen += sprintf(outs + outlen, "<div class='latexblock'>");

    char *outcompile;
    bool success;
    tie(success, outcompile) =
        compileLatex(katex_ctx, raw_input, span, LatexType::LatexTypeBlock);
    if (!success) { return false; }
    strcpy(outs + outlen, outcompile);
    outlen += strlen(outcompile);
    free(outcompile);

    outlen += sprintf(outs + outlen, "</div>");
    return true;
  }

  case BlockTm::Kind::Heading: {
    const BlockHeading *heading = (const BlockHeading *)t;
    const char *link = mkHeadingURL(raw_input, heading);
    const int h = min(4, 1 + heading->hnum);
    outlen += sprintf(outs + outlen, "<h%d>", h);
    outlen +=
        sprintf(outs + outlen, "<a id=%s href='#%s'> %s </a>", link, link, "§");
    const bool success = renderInlineLine(katex_ctx, prism_ctx, raw_input,
                                          heading->line, outlen, outs);
    outlen += sprintf(outs + outlen, "</h%d>", h);
    free((char *)link);
    return success;
  }

  case BlockTm::Kind::Paragraph: {
    const BlockParagraph *para = (const BlockParagraph *)t;
    bool success = true;
    if (para->wrap) { outlen += sprintf(outs + outlen, "<p>"); }
    for (ll i = 0; i < (ll)para->lines.size(); ++i) {
      if (i > 0) { outlen += sprintf(outs + outlen, "\n"); }
      success &= renderInlineLine(katex_ctx, prism_ctx, raw_input,
                                  para->lines[i], outlen, outs);
    }
    if (para->wrap) { outlen += sprintf(outs + outlen, "</p>"); }
    return success;
  }

  case BlockTm::Kind::List:
  case BlockTm::Kind::NumberedList: {
    const BlockList *list = (const BlockList *)t;
    const bool numbered = t->kind == BlockTm::Kind::NumberedList;
    bool success = true;
    outlen += sprintf(outs + outlen, numbered ? "<ol>" : "<ul>");
    for (const ListItemTm &item : list->items) {
      outlen += sprintf(outs + outlen, "<li>");
      for (const InlineLine &line : item.lines) {
        success &= renderInlineLine(katex_ctx, prism_ctx, raw_input, line,
                                    outlen, outs);
      }
      outlen += sprintf(outs + outlen, "</li>");
    }
    outlen += sprintf(outs + outlen, numbered ? "</ol>" : "</ul>");
    return success;
  }

  case BlockTm::Kind::Quote: {
    const BlockQuote *quote = (const BlockQuote *)t;
    bool success = true;
    outlen += sprintf(outs + outlen, "<blockquote>");
    // consecutive quote lines reflow as one paragraph (source linebreaks
    // are wrapping artifacts); an empty `>` line separates paragraphs.
    bool open = false;
    for (size_t k = 0; k < quote->lines.size(); ++k) {
      const Span &sp = quote->line_spans[k];
      bool blank = true;
      for (ll si = sp.begin.si; si < sp.end.si; ++si) {
        if (!isspace(raw_input[si])) { blank = false; break; }
      }
      if (blank) {
        if (open) {
          outlen += sprintf(outs + outlen, "</p>");
          open = false;
        }
        continue;
      }
      if (!open) {
        outlen += sprintf(outs + outlen, "<p>");
        open = true;
      } else {
        outlen += sprintf(outs + outlen, "\n");
      }
      success &= renderInlineLine(katex_ctx, prism_ctx, raw_input,
                                  quote->lines[k], outlen, outs);
    }
    if (open) { outlen += sprintf(outs + outlen, "</p>"); }
    outlen += sprintf(outs + outlen, "</blockquote>");
    return success;
  }
  }
  assert(false && "unreachable");
}

// TUFTE
// <body vlink="#660000" text="#000000" link="#CC0000"
//  bgcolor="#FFFFF3" alink="#660000">
const char html_preamble[] =
    "<!DOCTYPE html>"
    "<meta charset='UTF-8'>"
    "<html lang='en'>"
    "<head>"
    // ===viewport===
    "<meta name='viewport' content='width=device-width, initial-scale=1'>"
    // ===abcjs===
    "<script src='/abcjs/abcjs-basic-min.js'></script>"
    "<link rel='stylesheet' href='/abcjs/abcjs-audio.css' >"
    // ===RSS===
    "<link rel='alternate' type='application/rss+xml' href='/feed.rss' title='" "A universe of sorts'" "/>"
    // ===KateX===
    "<link rel='stylesheet' href='/katex/katex.min.css'"
    "    "
    "integrity='sha384-AfEj0r4/OFrOo5t7NnNe46zW/tFgW6x/"
    "bCJG8FqQCEo3+Aro6EYUG4+cU+KJWu/X'"
    "    crossorigin='anonymous'>"
    "<!-- The loading of KaTeX is deferred to speed up page rendering -->"
    // ===Prismjs===
    "<link rel='stylesheet' href='/prism/prism.css'>"
    // ===End KateX===
    "<title> A Universe of Sorts </title>"
    "<link rel='stylesheet' href='/css/stylesheet.css'>"
    // blog script
    "<script src='/script/blog.js'></script>"
    "</head>"
    "<body>"
    "<div class='container'>";

#define utterances_preamble                                                    \
  "<script src=\"https://utteranc.es/client.js\""                              \
  "        repo=\"bollu/bollu.github.io\""                                     \
  "        issue-term=\"pathname\""                                            \
  "        label=\"question\""                                                 \
  "        theme=\"github-light\""                                             \
  "        crossorigin=\"anonymous\""                                          \
  "        async>"                                                             \
  "</script>"



#define html_postamble \
   "</container>" \
   "</body>" \
   "</html>"

static const ll MAX_OUTPUT_BUF_LEN = (ll)1e9L;

char raw_input[MAX_CHARS];

bool is_h1(const BlockTm *t) {
  return t->kind == BlockTm::Kind::Heading && ((BlockHeading *)t)->hnum == 1;
}

// per-article info collected by splitting the block stream on H1s.
struct ArticleInfo {
  const BlockHeading *heading;
  const char *url;       // from mkHeadingURL; includes the /articles/ prefix.
  ll ix_start, ix_end;   // block range [ix_start, ix_end).
  const BlockMeta *meta; // nullptr if the article has no ```meta block.
};

vector<ArticleInfo> collectArticles(const vector<BlockTm *> &ts,
                                    const char *raw_input) {
  vector<ArticleInfo> articles;
  std::unordered_map<std::string, int> url_count;
  ll ix = 0;
  while (ix < (ll)ts.size() && !is_h1(ts[ix])) { ix++; }
  while (ix < (ll)ts.size()) {
    ArticleInfo info;
    info.heading = (const BlockHeading *)ts[ix];
    info.url = mkHeadingURL(raw_input, info.heading);
    info.ix_start = ix;
    info.meta = nullptr;

    ix++;
    while (ix < (ll)ts.size() && !is_h1(ts[ix])) { ix++; }
    info.ix_end = ix;

    // metadata is the block immediately after the heading (blank lines
    // produce no tokens).
    if (info.ix_start + 1 < info.ix_end &&
        ts[info.ix_start + 1]->kind == BlockTm::Kind::Meta) {
      info.meta = (const BlockMeta *)ts[info.ix_start + 1];
    }

    if (++url_count[info.url] > 1) {
      printf("WARNING: duplicate article URL |%s|; earlier article at this "
             "URL is overwritten.\n", info.url);
    }
    articles.push_back(info);
  }
  return articles;
}

// the heading's plain text, trimmed; used for the big-list labels.
std::string headingPlaintext(const char *raw_input,
                             const BlockHeading *heading) {
  const int BUFSIZE = (1 << 10);
  char buf[BUFSIZE];
  for (int i = 0; i < BUFSIZE; ++i) { buf[i] = 0; }
  ll len = 0;
  inlineLineToPlaintext(raw_input, heading->line, buf, len);
  std::string s(buf);
  const size_t b = s.find_first_not_of(" \t");
  if (b == std::string::npos) { return ""; }
  const size_t e = s.find_last_not_of(" \t");
  return s.substr(b, e - b + 1);
}

// "Big List of Funk Jazz Standards" -> "Funk Jazz Standards";
// "My Favourite APLisms" -> "APLisms";
// "Big List of Art and Paintings I Enjoy" -> "Art and Paintings".
std::string bigListLabel(const std::string &title) {
  static const char *prefixes[] = {"big list of ", "big lists of ",
                                   "big list ", "my favourite ",
                                   "my favorite ", "favourite ", "favorite ",
                                   "i like "};
  static const char *suffixes[] = {" i enjoy", " i like", " i admire"};
  std::string out = title;
  std::string lower = out;
  for (char &c : lower) { c = tolower(c); }
  for (const char *p : prefixes) {
    if (lower.rfind(p, 0) == 0) {
      out = out.substr(strlen(p));
      lower = lower.substr(strlen(p));
      break;
    }
  }
  for (const char *sfx : suffixes) {
    const size_t n = strlen(sfx);
    if (lower.size() > n && lower.compare(lower.size() - n, n, sfx) == 0) {
      out = out.substr(0, out.size() - n);
      break;
    }
  }
  return out;
}

// one row of the big-lists section: "<label> A · B · C" as plain links.
static ll writeListRow(const char *raw_input,
                       const vector<ArticleInfo> &articles,
                       const MetaStatus status, const char *label,
                       KEEP char *outs) {
  vector<std::pair<std::string, const ArticleInfo *>> rows;
  for (const ArticleInfo &a : articles) {
    if (a.meta && a.meta->status == status) {
      rows.push_back({bigListLabel(headingPlaintext(raw_input, a.heading)),
                      &a});
    }
  }
  if (rows.empty()) { return 0; }
  std::sort(rows.begin(), rows.end(),
            [](const std::pair<std::string, const ArticleInfo *> &a,
               const std::pair<std::string, const ArticleInfo *> &b) {
              std::string la = a.first, lb = b.first;
              for (char &c : la) { c = tolower(c); }
              for (char &c : lb) { c = tolower(c); }
              return la < lb;
            });

  ll outlen = 0;
  outlen += sprintf(outs + outlen,
                    "<div class='big-lists-row'>"
                    "<span class='big-lists-label'>%s</span> ", label);
  const char *sep = "";
  for (const auto &row : rows) {
    outlen += sprintf(outs + outlen, "%s<a href='%s.html'>%s</a>", sep,
                      row.second->url, row.first.c_str());
    sep = " · ";
  }
  outlen += sprintf(outs + outlen, "</div>");
  return outlen;
}

// the big-list and i-like articles, lifted out of the chronological list
// into plain lines of links. placed where the preamble's <!-- big-lists -->
// marker comment sits.
long long writeBigListsHTML(const char *raw_input,
                            const vector<ArticleInfo> &articles,
                            KEEP char *outs) {
  ll outlen = 0;
  outlen += writeListRow(raw_input, articles, MetaStatus::BigList,
                         "Big lists of", outs + outlen);
  outlen += writeListRow(raw_input, articles, MetaStatus::ILike, "I like",
                         outs + outlen);
  return outlen;
}

// returns number of characters written.
// homepage: the done/draft filter and the (filterable) chronological post
// list; big lists live in their own section (writeBigListsHTML).
long long writeHomepageTOC(duk_context *katex_ctx, duk_context *prism_ctx,
                           const char *raw_input,
                           const vector<ArticleInfo> &articles,
                           KEEP char *outs) {
  printf("===writing homepage TOC===\n");
  ll outlen = 0;

  // ===filter bar===
  outlen += sprintf(outs + outlen,
      "<div id='filter-bar'>"
      "<div id='status-filter'>"
      "<button class='pill status-pill is-active' data-status-filter='all'>all</button>"
      "<button class='pill status-pill' data-status-filter='technical-note'>technical notes</button>"
      "<button class='pill status-pill' data-status-filter='essay'>essays</button>"
      "<button class='pill status-pill' data-status-filter='scratch'>scratch</button>"
      "<a class='garage-door' "
      "href='https://notes.andymatuschak.org/About_these_notes"
      "?stackedNotes=zCMhncA1iSE74MKKYQS5PBZ'>"
      "Work With The Garage Door Open</a>"
      "<span class='garage-door-author'> — Andy Matuschak</span>"
      "</div>"
      "</div>");

  // ===post list (big lists live in their own section above)===
  outlen += sprintf(outs + outlen, "<ol reversed id='post-list'>");
  for (const ArticleInfo &a : articles) {
    if (a.meta && (a.meta->status == MetaStatus::BigList ||
                   a.meta->status == MetaStatus::ILike)) {
      continue;
    }
    const char *status = "scratch";
    if (a.meta && a.meta->status == MetaStatus::TechnicalNote) {
      status = "technical-note";
    } else if (a.meta && a.meta->status == MetaStatus::Essay) {
      status = "essay";
    }
    char year[5] = {0};
    if (a.meta && a.meta->created[0]) { strncpy(year, a.meta->created, 4); }

    outlen += sprintf(outs + outlen,
        "<li class='post-row' data-status='%s' data-year='%s'>", status, year);
    outlen += sprintf(outs + outlen, "<a href='%s.html' class='post-title'>", a.url);
    renderInlineLine(katex_ctx, prism_ctx, raw_input, a.heading->line,
                     outlen, outs);
    outlen += sprintf(outs + outlen, "</a>");
    outlen += sprintf(outs + outlen, "<span class='post-meta'>");
    if (year[0]) {
      outlen += sprintf(outs + outlen, "<span class='post-year'>%s</span>", year);
    }
    outlen += sprintf(outs + outlen,
        "<span class='status-label status-%s'>%s</span>", status,
        strcmp(status, "technical-note") == 0 ? "technical" : status);
    outlen += sprintf(outs + outlen, "</span></li>");
  }
  outlen += sprintf(outs + outlen, "</ol>");
  return outlen;
}

struct RSS {

  // https://en.wikipedia.org/wiki/Character_encodings_in_HTML#XML_character_references
  static void writeEscapedCharacter(char c, std::string &out) {
    if (c == '<') {
      out += " &lt; ";
    } else if (c == '>') {
      out += " &gt; ";
    } else if (c == '\"') {
      out += " &quot; ";
    } else if (c == '\'') {
      out += " &apos; ";
    } else if (c == '&') {
      out += "&amp;";
    } else {
      out.push_back(c);
    }
  }

  // the heading's inline terms as escaped plain text.
  static void mkRSSTitle(const char *raw_input, const InlineLine &line,
                         std::string &out) {
    for (const InlineTm *t : line) {
      switch (t->kind) {
      case InlineTm::Kind::Italic:
        mkRSSTitle(raw_input, ((const InlineItalic *)t)->items, out);
        break;
      case InlineTm::Kind::Link:
        mkRSSTitle(raw_input, ((const InlineLink *)t)->text, out);
        break;
      case InlineTm::Kind::Code:
      case InlineTm::Kind::Latex:
        // strip the delimiter characters.
        for (ll i = t->span.begin.si + 1; i < t->span.end.si - 1; ++i) {
          writeEscapedCharacter(raw_input[i], out);
        }
        break;
      case InlineTm::Kind::Text:
        for (ll i = t->span.begin.si; i < t->span.end.si; ++i) {
          writeEscapedCharacter(raw_input[i], out);
        }
        // at the end of raw text, write a space.
        writeEscapedCharacter(' ', out);
        break;
      case InlineTm::Kind::Image:
        break; // images contribute nothing to RSS titles.
      }
    }
  }
  // "YYYY-MM-DD" -> RFC-822 "Mon, 12 Mar 2024 00:00:00 +0000"; false on
  // failure. RSS 2.0 requires RFC-822 dates.
  static bool rfc822FromISODate(const char *iso, char *out, size_t outsz) {
    int y, m, d;
    if (sscanf(iso, "%4d-%2d-%2d", &y, &m, &d) != 3) { return false; }
    struct tm tm = {};
    tm.tm_year = y - 1900;
    tm.tm_mon = m - 1;
    tm.tm_mday = d;
    if (timegm(&tm) == (time_t)-1) { return false; } // normalizes tm_wday.
    return strftime(out, outsz, "%a, %d %b %Y 00:00:00 +0000", &tm) > 0;
  }

  // https://www.mnot.net/rss/tutorial/
  static void writeRSSFeed(KEEP FILE *frss, KEEP const char *raw_input,
                           const vector<ArticleInfo> &articles) {
    assert(frss != nullptr);
    // https://www.mnot.net/rss/tutorial/
    fprintf(frss, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    fprintf(frss, "<rss version=\"2.0\">\n");
    fprintf(frss, "<channel>\n");
    fprintf(frss, "<title>A universe of sorts</title>\n");
    fprintf(frss, "<link>https://bollu.github.io/</link>\n");
    fprintf(frss, "<description>%s</description>\n",
            CONFIG_WEBSITE_RSS_DESCRIPTION);

    for (const ArticleInfo &article : articles) {
      // <item>
      // <title>News for September the Second</title>
      // <link>http://example.com/2002/09/01</link>
      // <description>other things happened today</description>
      // </item>

      std::string title;
      mkRSSTitle(raw_input, article.heading->line, title);

      fprintf(frss, "  <item>\n");
      fprintf(frss, "    <title>%s</title>\n", title.c_str());
      // tell the aggregators that we are using RSS 2.0
      // article.url already begins with /articles/.
      fprintf(frss, "    <guid>%s%s.html</guid>\n",
              CONFIG_WEBSITE_URL_NO_TRAILING_SLASH, article.url);
      fprintf(frss, "    <link>%s%s.html</link>\n",
              CONFIG_WEBSITE_URL_NO_TRAILING_SLASH, article.url);
      if (article.meta && article.meta->created[0]) {
        char rfc822[64];
        if (rfc822FromISODate(article.meta->created, rfc822, sizeof(rfc822))) {
          fprintf(frss, "    <pubDate>%s</pubDate>\n", rfc822);
        }
      }
      fprintf(frss, "  </item>\n");
    }
    // end the file.
    fprintf(frss, "</channel>\n");
    fprintf(frss, "</rss>");
  }
};

int main(int argc, char **argv) {
  assert(argc == 1 && "usage: builder (options are by changing CONFIG_* "
                      "variables and recompiling");

  // 1. Initialize Duck context for katex
  // --------------------------
  duk_context *katex_ctx = load_katex();
  assert(katex_ctx != nullptr && "Unable to setup katex context");

  // 1. Initialize Duck context for prismjs
  // --------------------------
  duk_context *prism_ctx = nullptr;
  {
    FILE *fprism = fopen(CONFIG_PRISM_PATH, "rb");
    if (fprism == nullptr) {
      fprintf(stderr, "ERROR: unable to find prism at path |%s|\n",
              CONFIG_PRISM_PATH);
      assert(false && "unable to open prism.js");
    }

    fseek(fprism, 0, SEEK_END);
    const ll len = ftell(fprism);
    fseek(fprism, 0, SEEK_SET);
    char *js = (char *)calloc(sizeof(char), len + 10);

    const ll nread = fread(js, 1, len, fprism);
    assert(nread == len);
    fclose(fprism);
    prism_ctx = duk_create_heap_default();

    duk_push_string(prism_ctx, "prism.min.js");
    // compile prism
    printf("===compiling prism...===\n");
    if (duk_pcompile_lstring_filename(prism_ctx, 0, js, len) != 0) {
      fprintf(stderr, "===prism.min.js compliation failed===\n%s\n===\n",
              duk_safe_to_string(prism_ctx, -1));
      assert(false && "unable to compile prism.min.js");
    }

    // run prism to get the global.prism object
    if (duk_pcall(prism_ctx, 0) != 0) {
      fprintf(stderr, "===prism.min.js execution failed===\n%s\n===\n",
              duk_safe_to_string(prism_ctx, -1));
      assert(false && "unable to execute prism.min.js");
    }

    printf("===loading prism...===\n");
    if (duk_peval_string(prism_ctx, "Prism") != 0) {
      fprintf(stderr,
              "====prism.min.js: unable to grab prism object===\n%s\n===\n",
              duk_safe_to_string(prism_ctx, -1));
      assert(false && "unable to find the prism object");
    }
  }
  assert(prism_ctx != nullptr && "Unable to setup duck context for prism");

  // 2. Open markdown file
  // ---------------------
  FILE *fin = fopen(CONFIG_INPUT_MARKDOWN_PATH, "rb");
  if (fin == nullptr) {
    printf("unable to open file: |%s|. Please set |CONFIG_INPUT_MARKDOWN_PATH| "
           "in the source code.\n",
           CONFIG_INPUT_MARKDOWN_PATH);
    return -1;
  }

  fseek(fin, 0, SEEK_END);
  const ll len = ftell(fin);
  fseek(fin, 0, SEEK_SET);
  assert(len < MAX_CHARS);
  cout << "===Input length: |" << len << "|===\n";

  const ll nread = fread(raw_input, 1, len, fin);
  assert(nread == len);

  // phase A: block structure; phase B: inline structure.
  vector<BlockTm *> ts = parseBlocks(raw_input, nread);
  parseInlines(ts, raw_input, nread);
  cout << "===Done parsing; Emitting HTML...===\n";

  const vector<ArticleInfo> articles = collectArticles(ts, raw_input);

  // index of the latest <h1> tag.
  ll ix_h1 = 0;
  
  // ===make output directories===

  struct stat st = {0};
  if (stat(OUTPUT_ROOT_DIR_TRAILING_SLASH, &st) == -1) {
      mkdir(OUTPUT_ROOT_DIR_TRAILING_SLASH, 0700);
  }
  if (stat(OUTPUT_ARTICLES_DIR_TRAILING_SLASH, &st) == -1) {
      if(mkdir(OUTPUT_ARTICLES_DIR_TRAILING_SLASH, 0700) == -1) {
        printf("Error making directory |%s| %s\n", 
          OUTPUT_ARTICLES_DIR_TRAILING_SLASH,
          strerror(errno));
      };
  }


  // ===write out index.html===
  {
    // seek till the first <h1>: put all that data in index.html
    while (ix_h1 < (ll)ts.size() && !is_h1(ts[ix_h1])) {
      ix_h1++;
    }
    cout << "===Writing index.html===\n";
    // [0, ix_h1) stays in index.html

    char *index_html_buf = (char *)calloc(MAX_OUTPUT_BUF_LEN, sizeof(char));
    ll outlen = 0;
    outlen += sprintf(index_html_buf + outlen, "%s", html_preamble);

    // the preamble's <!-- big-lists --> marker comment places the big-list
    // section; without a marker it lands just above the post list.
    bool biglists_emitted = false;
    for (int i = 0; i < ix_h1; ++i) {
      if (ts[i]->kind == BlockTm::Kind::Comment &&
          std::string(raw_input + ts[i]->span.begin.si,
                      raw_input + ts[i]->span.end.si)
                  .find("big-lists") != std::string::npos) {
        outlen +=
            writeBigListsHTML(raw_input, articles, index_html_buf + outlen);
        biglists_emitted = true;
        continue;
      }
      renderBlock(katex_ctx, prism_ctx, raw_input, ts[i], outlen,
                  index_html_buf);
    }

    // ===write out table of contents===
    if (!biglists_emitted) {
      outlen += writeBigListsHTML(raw_input, articles, index_html_buf + outlen);
    }
    outlen += writeHomepageTOC(katex_ctx, prism_ctx, raw_input, articles,
                               index_html_buf + outlen);
    outlen += sprintf(index_html_buf + outlen, "%s", html_postamble);

    char index_html_path[1024];
    sprintf(index_html_path, "%sindex.html", OUTPUT_ROOT_DIR_TRAILING_SLASH);
    FILE *f = fopen(index_html_path, "wb");
    if (f == nullptr) {
      fprintf(stdout, "===unable to open HTML file: |%s|===", index_html_path);
      return 1;
    }
    assert(f != nullptr);
    fwrite(index_html_buf, 1, strlen(index_html_buf), f);
    fclose(f);
  }

  // create path for index.html
  // if (mkdir("OUTPUT_ARTICLES_DIR_TRAILING_SLASH", S_IRWXU | S_IRWXG | S_IRWXO) == -1) {
  //         printf("Error: %s\n", strerror(errno));
  // }


  // ===write out all of the other .html files===
  while (ix_h1 < (ll)ts.size()) {
    const int ix_start = ix_h1;
    assert(is_h1(ts[ix_start]));
    const BlockHeading *heading = (const BlockHeading *)ts[ix_start];

    ix_h1++;
    while (ix_h1 < (ll)ts.size() && !is_h1(ts[ix_h1])) {
      ix_h1++;
    }
    const char *url = mkHeadingURL(raw_input, heading);

    // TODO: find some easy way to print WTF is the data in the heading.
    cout << "===Writing [" << url << ".html]===\n";

    char *outbuf = (char *)calloc(MAX_OUTPUT_BUF_LEN, sizeof(char));
    ll outlen = 0;
    outlen += sprintf(outbuf + outlen, "%s", html_preamble);
    bool success = true;
    ll i = ix_start;
    // peek the meta block (it follows the heading) for the kicker, the
    // layout, and the essay drop-cap class.
    const BlockMeta *meta = nullptr;
    if (ix_start + 1 < ix_h1 &&
        ts[ix_start + 1]->kind == BlockTm::Kind::Meta) {
      meta = (const BlockMeta *)ts[ix_start + 1];
    }
    const MetaStatus status = meta ? meta->status : MetaStatus::Scratch;

    // ===kicker: the magazine-style section label above the headline===
    const char *kicker = nullptr, *kicker_cls = nullptr;
    switch (status) {
    case MetaStatus::TechnicalNote:
      kicker = "technical note"; kicker_cls = "technical-note"; break;
    case MetaStatus::Essay: kicker = "essay"; kicker_cls = "essay"; break;
    case MetaStatus::Scratch: kicker = "scratch"; kicker_cls = "scratch"; break;
    case MetaStatus::BigList:
      kicker = "big list"; kicker_cls = "big-list"; break;
    case MetaStatus::ILike: kicker = "i like"; kicker_cls = "big-list"; break;
    }
    outlen += sprintf(outbuf + outlen,
                      "<div class='kicker kicker-%s'>%s</div>", kicker_cls,
                      kicker);

    // the title and meta/date line span the full width; the body below
    // flows in the article-body wrapper (two-column by default).
    success &=
        renderBlock(katex_ctx, prism_ctx, raw_input, ts[i], outlen, outbuf);
    i++;
    if (meta) {
      success &=
          renderBlock(katex_ctx, prism_ctx, raw_input, ts[i], outlen, outbuf);
      i++;
    }

    std::string body_cls = "article-body";
    if (!meta || meta->layout == LayoutKind::TwoColumn) {
      body_cls += " two-column";
    }
    if (status == MetaStatus::Essay) { body_cls += " status-essay"; }
    outlen +=
        sprintf(outbuf + outlen, "<div class='%s'>", body_cls.c_str());
    for (; i < ix_h1; ++i) {
      success &=
          renderBlock(katex_ctx, prism_ctx, raw_input, ts[i], outlen, outbuf);
    }
    // ===tailpiece: the end-of-article mark===
    outlen += sprintf(outbuf + outlen, "<div class='tailpiece'>❦</div>");
    outlen += sprintf(outbuf + outlen, "</div>");

    if (!success) {
      fprintf(stdout, "===ERROR: compile [%s] failed. skipping. ", url);
      assert(false && "failed compilation");
      continue;
    }

    outlen += sprintf(outbuf + outlen, "<div id=\"footer\">");

    if (ix_start > 1) {
      int ix = ix_start - 1;
      while(ix >= 0 && !is_h1(ts[ix])) { ix--; } 
      if (ix >= 0  && is_h1(ts[ix])) {
        const BlockHeading *prev = (const BlockHeading *)ts[ix];
        outlen += sprintf(outbuf + outlen, "<a class=\"footer-item\" href=\"%s.html\"> Newer </a>",
            mkHeadingURL(raw_input, prev));
      }
    } 
    outlen += sprintf(outbuf + outlen,
         "  ৪ <a href=\"/\" class=\"footer-item\"> Blog </a>  ৪  ");

    if (ix_h1 < ts.size() - 1) {
      int ix = ix_start +1;
      while(ix < (ll)ts.size() && !is_h1(ts[ix])) { ix++; }
      if (ix < (ll) ts.size() && is_h1(ts[ix])) {
        const BlockHeading *next = (const BlockHeading *)ts[ix];
        outlen += sprintf(outbuf + outlen, "<a class=\"footer-item\" href=\"%s.html\"> Older </a>", 
            mkHeadingURL(raw_input, next));
      }
    }

    outlen += sprintf(outbuf + outlen, "</div>");


    outlen += sprintf(outbuf + outlen, utterances_preamble);
    outlen += sprintf(outbuf + outlen, html_postamble);

    // [ix_start, ix_h1) contains the new article
    char html_path[1024];

    sprintf(html_path, "%s%s.html", OUTPUT_ROOT_DIR_TRAILING_SLASH, url);
    fprintf(stdout, "....writing to |%s|\n", html_path);
    FILE *f = fopen(html_path, "wb");
    if (f == nullptr) {
      fprintf(stdout, "===unable to open HTML file: |%s|===", html_path);
      return 1;
    }
    assert(f != nullptr);
    fwrite(outbuf, 1, strlen(outbuf), f);
    fclose(f);
  }

  // === write out RSS ===
  char rss_feed_path[1024];
  sprintf(rss_feed_path, "%sfeed.rss",
          OUTPUT_ROOT_DIR_TRAILING_SLASH);
  FILE *frss = fopen(rss_feed_path, "wb");
  if (frss == nullptr) {
    fprintf(stdout, "===unable to open RSS file: |%s|===\n", rss_feed_path);
    return 1;
  }

  RSS::writeRSSFeed(frss, raw_input, articles);
  fclose(frss);

  return 0;
}
