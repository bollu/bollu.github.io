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
// #include <sys/resource.h>
// #include <sys/wait.h>
#include <tuple>
// #include <unistd.h>
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

#define CONFIG_INPUT_MARKDOWN_PATH  (BLOG_ROOT_FOLDER_TRAILING_SLASH "README.txt")
#define CONFIG_KATEX_PATH  (BLOG_ROOT_FOLDER_TRAILING_SLASH "katex/katex.min.js")
#define CONFIG_PRISM_PATH (BLOG_ROOT_FOLDER_TRAILING_SLASH "prism/prism.js")
#define OUTPUT_ROOT_DIR_TRAILING_SLASH BLOG_ROOT_FOLDER_TRAILING_SLASH "out/"
#define OUTPUT_ARTICLES_URL_TRAILING_SLASH "articles/"
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


// indent for logging to tell which function is calling what.
struct Logger {
  static int G_LOG_INDENT;
  const int indent;
  Logger() : indent(G_LOG_INDENT) {
    G_LOG_INDENT++;
    if (indent > 40) {
      assert(false &&
             "indent more than 40 levels deep; you sure this is correct?");
    }
  }
  ~Logger() { G_LOG_INDENT--; }
  void print(std::ostream &o) {
    for (int i = 0; i < indent; ++i) {
      o << " ";
    }
  }
};
int Logger::G_LOG_INDENT = 1;

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

enum class TT {
  Comment,
  RawText,
  HTML,
  LatexBlock,
  LatexInline,
  CodeInline,
  CodeBlock,
  LineBreak,
  Link,
  List,
  TListNumbered,
  InlineGroup,
  Heading,
  Italic,
  Bold,
  Quote,
  Undefined,
};

std::ostream &operator<<(std::ostream &o, const TT &ty) {
  switch (ty) {
  case TT::Comment:
    return o << "COMMENT";
  case TT::HTML:
    return o << "HTML";
  case TT::LineBreak:
    return o << "LineBreak";
  case TT::LatexBlock:
    return o << "LatexBlock";
  case TT::LatexInline:
    return o << "LatexInline";
  case TT::CodeBlock:
    return o << "CodeBlock";
  case TT::CodeInline:
    return o << "CodeInline";
  case TT::RawText:
    return o << "RAW";
  case TT::Undefined:
    return o << "UNDEFINED";
  case TT::Link:
    return o << "LINK";
  case TT::List:
    return o << "LIST";
  case TT::TListNumbered:
    return o << "LISTNUMBERED";
  case TT::InlineGroup:
    return o << "INLINEGROUP";
  case TT::Heading:
    return o << "HEADING";
  case TT::Italic:
    return o << "ITALIC";
  case TT::Bold:
    return o << "BOLD";
  case TT::Quote:
    return o << "Quote";
  }
  assert(false && "unreachable");
};

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
  fprintf(f, "\nvvvraw filevvv\n");
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
    fprintf(f, "^^^raw file^^^\n");
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
  fprintf(f, "^^^raw file^^^\n");
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

// T type, optional string data.
struct T {
  TT ty = TT::Undefined;
  Span span = Span(L(-1, -1, -1), L(-1, -1, -1));

  T(TT ty, Span span) : ty(ty), span(span){};
  T(){};
};

std::ostream &operator<<(std::ostream &o, const T &t) {
  return o << t.ty << "[" << t.span << "]";
}

// TLink for link information.
struct TLink : public T {
  // link is taken ownership of.
  TLink(Span span, T *text, const char *link)
      : T(TT::Link, span), text(text), link(link){};
  T *text;
  const char *link;
};

Span mkTokensSpan(L startloc, const vector<T *> &items) {
  if (items.size() == 0) { return Span(startloc, startloc); };
  assert(items.size() > 0);
  Span s = items[0]->span;
  assert(s.begin.si >= startloc.si);
  for (int i = 1; i < (int)items.size(); ++i) {
    Span next = items[i]->span;

    if (next.begin.si < s.end.si) {
      cerr << "merging: " << *items[i - 1] << " | " << *items[i] << "\n";
    }
    assert(next.begin.si >= s.end.si);
    s = Span(s.begin, next.end);
  }
  return s;
}

struct TList : public T {
  vector<T *> items;
  TList(L begin, vector<T *> items) : T(TT::List, mkTokensSpan(begin, items)), items(items){};
};

struct TListNumbered : public T {
  vector<T *> items;
  TListNumbered(L begin, vector<T *> items)
      : T(TT::TListNumbered, mkTokensSpan(begin, items)), items(items){};
};

struct TCodeBlock : public T {
  TCodeBlock(Span span, KEEP const char *langname)
      : T(TT::CodeBlock, span), langname(strdup(langname)) {}
  char *langname;
};

struct TInlineGroup : public T {
  TInlineGroup(L begin, vector<T *> items)
      : T(TT::InlineGroup, mkTokensSpan(begin, items)), items(items) {}
  vector<T *> items;
};

struct THeading : public T {
  THeading(int hnum, Span span, T *item)
      : T(TT::Heading, span), hnum(hnum), item(item){};
  int hnum;
  T *item;
};

struct TItalic : public T {
  TItalic(Span span, T *item) : T(TT::Italic, span), item(item){};
  T *item;
};

struct TBold : public T {
  TBold(Span span, T *item) : T(TT::Bold, span), item(item){};
  T *item;
};

struct TQuote : public T {
  TQuote(Span span, vector<T *> items) : T(TT::Quote, span), items(items){};
  vector<T *> items;
};

// we may have to stop at `\n` for a heading. Argh.
bool is_char_special_inline_token(char c) {
  return c == '*' || c == '[' || c == ']' || c == '$' || c == '`' || c == '_' ||
         c == '\n';
}

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

T *tokenizeLink(const char *s, const ll len, const L opensq);
T *tokenizeLineFragment(const char *s, const ll len, const L lbegin) {
  Logger logger;
  logger.print(cerr);
  // cerr << "tokenizeLineFragment(" << lbegin << ")\n";
  assert(lbegin.si < len);

  T *linkt = nullptr;
  if (s[lbegin.si] == '[' &&
      (linkt = tokenizeLink(s, len, lbegin)) != nullptr) {
    logger.print(cerr);
    cerr << "tokenizeLineFragment:link(" << lbegin << ")\n";
    return linkt;
  } else if (s[lbegin.si] == '`') {
    logger.print(cerr);
    cerr << "tokenizeLineFragment:code(" << lbegin << ")\n";
    L lcur = lbegin.next('`');
    // TODO: fix error message here.
    lcur = strconsume(lcur, s, "`", "unclosed inline code block `...`");

    if (lbegin.line != lcur.line) {
      printf_err_loc(lbegin, s,
                "inline code block `...` not allowed to "
                "be on two different lines. "
                "Found on lines: (%d:%d---%d:%d)",
                lbegin.line, lbegin.col, lcur.line, lcur.col);
      assert(false && "inline code block `...` on two different lines.");
    }

    return new T(TT::CodeInline, Span(lbegin, lcur));
  } else if (s[lbegin.si] == '$') {
    cerr << "tokenizeLineFragment:$(" << lbegin << ")\n";
    L lcur = lbegin.next('$');
    // TODO: fix error message here.
    lcur = strconsume(lcur, s, "$", "unclosed inline latex block $");

    if (lbegin.line != lcur.line) {
      // inline latex block not allowed to be on two different lines.
      // TODO: add color to the line!
      printf_err_span(Span(lbegin, lcur), s,
                "inline latex block not allowed to be on two different lines.");
      assert(false && "inline latex block on two different lines.");
    }

    return new T(TT::LatexInline, Span(lbegin, lcur));
  }
  /*
  else if (lbegin.si < len - 1 &&
             (s[lbegin.si] == '*' || s[lbegin.si] == '_') &&
             s[lbegin.si + 1] == s[lbegin.si]) {
    logger.print(cerr);
    cerr << "tokenizeLineFragment:bold(" << lbegin << ")\n";
    logger.print(cerr);
    const char c = s[lbegin.si];
    const char delim[3] = {c, c, 0};

    L lcur = lbegin.next(delim);
    vector<T *> toks;
    while (1) {
      T *t = tokenizeLineFragment(s, len, lcur);
      toks.push_back(t);
      lcur = t->span.end;
      if (lcur.si == len) {
        printf_err_loc(lbegin, s, "unmatched bold demiliter: |%s|.", delim);
        assert(false && "unmatched bold delimiter");
      }

      assert(lcur.si < len);

      if (s[lcur.si] == '\n') {
        printf_err_loc(lbegin, s, "bold emphasis spread across multiple lines!");
        assert(false && "bold spread across multiple lines");
      }

      if (strpeek(s + lcur.si, delim)) {
        break;
      }
    }

    assert(strpeek(s + lcur.si, delim));
    lcur = lcur.next(delim);
    return new TBold(Span(lbegin, lcur), new TInlineGroup(toks));

  }
  */
  // This ordering is important; bold MUST be checked first.
  else if (s[lbegin.si] == '*' || s[lbegin.si] == '_') {
    logger.print(cerr);
    cerr << "tokenizeLineFragment:italic(" << lbegin << ")\n";
    const char delim = s[lbegin.si];
    L lcur = lbegin.next(delim);
    vector<T *> toks;
    while (1) {
      T *t = tokenizeLineFragment(s, len, lcur);
      assert(t);
      toks.push_back(t);

      lcur = t->span.end;
      if (lcur.si == len) {
        printf_err_loc(lbegin, s, "unmatched italic demiliter: |%c|.", delim);
        assert(false && "unmatched italic delimiter");
      }

      assert(lcur.si < len);

      if (s[lcur.si] == '\n') {
        printf_err_loc(lbegin, s, "italic emphasis spread across multiple lines!");
        assert(false && "italic spread across multiple lines");
      }

      if (s[lcur.si] == delim) {
        break;
      }
    }

    assert(s[lcur.si] == delim);
    lcur = lcur.next(delim);
    return new TItalic(Span(lbegin, lcur), new TInlineGroup(lbegin, toks));
  } else {
    logger.print(cerr);
    cerr << "tokenizeLineFragment:raw(" << lbegin << ")\n";
    L lcur = lbegin;
    while (1) {
      lcur = lcur.next(s[lcur.si]);
      const char c = s[lcur.si];
      if (c == '*' || c == '[' || c == ']' || c == '$' || c == '`' ||
          c == '_' || c == '\n' || c == '\0') {
        break;
      }
    }
    return new T(TT::RawText, Span(lbegin, lcur));
  }
}

// consumes INCLUDING newline.
T *tokenizeInlineLine(const char *s, const ll len, const L lbegin) {
  Logger logger;
  logger.print(cerr);
  cerr << "tokenizeInlineLine(" << lbegin << ")\n";
  vector<T *> toks;
  L lcur = lbegin;
  while (1) {
    if (s[lcur.si] == '\n') {
      // consume newline.
      lcur.si += 1;
      break;
    }
    T *t = tokenizeLineFragment(s, len, lcur);
    lcur = t->span.end;
    toks.push_back(t);
  }
  return new TInlineGroup(lbegin, toks);
};

// tokenize those strings that can only occur "inside" an inline context,
// so only:
// - raw text
// - inline math
// - bold/italic/underline
// This is _wrong_, because we may have a ']' till sth else.
// TODO: add an assert to check that a link does not contain a
// link inside it. eg: [foo is[bar](barlink) xxx](foolink)
T *tokenizeLink(const char *s, const ll len, const L opensq) {
  cerr << "tokenizeLink(" << opensq << ")\n";
  assert(opensq.si < len);
  assert(s[opensq.si] == '[');
  L lcur = opensq.next('[');

  vector<T *> toks;
  while (1) {
    T *t = tokenizeLineFragment(s, len, lcur);
    lcur = t->span.end;
    toks.push_back(t);
    if (s[lcur.si] == ']') {
      break;
    }

    // we have an `[` without an accompanying `]` on the same line, so this
    // can't be a link...
    if (s[lcur.si] == '\n') {
      return nullptr;
    }
  };
  assert(s[lcur.si] == ']');

  const L closesq = lcur;
  const L openround = closesq.next(s[closesq.si]);
  if (s[openround.si] != '(') {
    return nullptr;
  };

  L closeround = openround;
  while (s[closeround.si] != ')' && s[closeround.si] != '\0') {
    closeround = closeround.next(s[closeround.si]);
  }

  if (s[closeround.si] != ')') {
    return nullptr;
  } else {
    closeround = closeround.next(')');
  }

  char *link = (char *)calloc((closeround.si - openround.si), sizeof(char));
  for (int i = 0, j = openround.si + 1; s[j] != ')'; ++i, ++j) {
    link[i] = s[j];
  }

  // TODO: change `tokenize` to `tokenizeInline` when the time is right.
  // std::tie(newline, text) = tokenize(s, closesq.si - opensq.si, opensq,
  // false); text =
  T *text = new TInlineGroup(opensq, toks);
  return new TLink(Span(opensq, closeround), text, link);
}

// Tokenizes a list hyphen. s[lhyphen] == '-';
T *tokenizeHyphenListItem(const char *s, const ll len, const L lhyphen) {
  Logger logger;
  logger.print(cerr);
  cerr << "tokenizeHyphenListItem(" << lhyphen << ")\n";
  assert(s[lhyphen.si] == '-');
  L lcur = lhyphen.next('-');

  vector<T *> toks;
  while (1) {
    T *t = tokenizeInlineLine(s, len, lcur);
    toks.push_back(t);
    lcur = t->span.end;
    assert(lcur.si == len || s[lcur.si] == '\n');
    if (lcur.si == len) {
      break;
    } else {
      // 1. consume the newline.
      lcur = lcur.next(s[lcur.si]);

      // decide if we continue the hyphen.
      if (s[lcur.si] != ' ' && s[lcur.si] != '\n' && s[lcur.si] != '-') {
        printf_err_loc(lcur, s,
                  "ERROR: list hyphen must either have (1) new aligned text, "
                  "(2) two newlines, (3) a new list hyphen.");
        printf_err_loc(
            lhyphen, s,
            "ERROR: incorrectly terminated list hyphen (started here)...");
        assert(false && "incorrectly terminated list hyphen");
      } else if (s[lcur.si] == '\n' || s[lcur.si] == '-') {
        break;
      } else {
        lcur = consumeIntraLineWhitespace(s, lcur);
        if (s[lcur.si] == '\n') {
          printf_err_loc(lcur, s,
                    "ERROR: list hyphen has incorrect white space ending in a "
                    "newline after it");
          printf_err_loc(
              lhyphen, s,
              "ERROR: incorrectly terminated list hyphen (started here)...");
          assert(false && "incorrect whitespace-like-line after list hyphen");
        }

        // we have whitespace followed by characters. good, continue.
        continue;
      }
    }
  }
  return new TInlineGroup(lhyphen, toks);
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

// we are assuming that this is called on the *first* list item that
// has been seen.
// s[lhyphen] == start of number;
T *tokenizeNumberedListItem(const char *s, const ll len, const L lhyphen,
                            const ll curnum) {

  Logger logger;
  logger.print(cerr);
  cerr << "tokenizeNumberedListItem(" << lhyphen << ")\n";

  char curitem[7];
  sprintf(curitem, "%lld.", curnum);
  if (!strpeek(s + lhyphen.si, curitem)) {
    printf_err_loc(lhyphen, s, "Expected list item to start with number: |%lld|",
              curnum);
    assert(false && "list item not respecting numbering.");
  }

  assert(strpeek(s + lhyphen.si, curitem));
  L lcur = lhyphen.next(curitem);

  vector<T *> toks;
  while (1) {
    T *t = tokenizeInlineLine(s, len, lcur);
    toks.push_back(t);
    lcur = t->span.end;
    assert(lcur.si == len || s[lcur.si] == '\n');
    if (lcur.si == len) {
      break;
    } else {
      // 1. consume the newline.
      lcur = lcur.next(s[lcur.si]);

      // decide if we continue the hyphen.
      if (s[lcur.si] != ' ' && s[lcur.si] != '\n' &&
          !isNumberedListBegin(s, len, lcur)) {
        printf_err_loc(lcur, s,
                  "ERROR: list hyphen must either have (1) new aligned text, "
                  "(2) two newlines, (3) a new numbered list beginning.");
        printf_err_loc(
            lhyphen, s,
            "ERROR: incorrectly terminated list hyphen (started here)...");
        assert(false && "incorrectly terminated list hyphen");
      } else if (s[lcur.si] == '\n' || isNumberedListBegin(s, len, lcur)) {
        break;
      } else {
        lcur = consumeIntraLineWhitespace(s, lcur);
        if (s[lcur.si] == '\n') {
          printf_err_loc(lcur, s,
                    "ERROR: list hyphen has incorrect white space ending in a "
                    "newline after it");
          printf_err_loc(
              lhyphen, s,
              "ERROR: incorrectly terminated list hyphen (started here)...");
          assert(false && "incorrect whitespace-like-line after list hyphen");
        }

        // we have whitespace followed by characters. good, continue.
        continue;
      }
    }
  }
  return new TInlineGroup(lhyphen, toks);
}

// quotes.
T *tokenizeQuoteItem(const char *s, const ll len, const L lquote) {
  cerr << "tokenizeQuoteItem(" << lquote << ")\n";
  assert(s[lquote.si] == '>');
  L lcur = lquote.next('>');
  vector<T *> toks;
  while (1) {
    T *t = tokenizeInlineLine(s, len, lcur);
    toks.push_back(t);
    lcur = t->span.end;
    assert(s[lcur.si] == '\n');
    if (lcur.si + 1 < len && s[lcur.si + 1] == '>') {
      lcur = lcur.next("\n>");
      continue;
    }
    break;
  }
  return new TQuote(Span(lquote, lcur), toks);
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
T *tokenizeBlock(const char *s, const ll len, const L lbegin) {
  Logger logger;
  logger.print(cerr);
  cerr << "tokenizeBlock(" << lbegin << ")\n";
  assert(lbegin.si < len);
  L lcur = lbegin;

  if (lcur.si < len - 1 && s[lcur.si] == '\n' && s[lcur.si + 1] == '\n') {
    while (lcur.si < len - 1 &&
           (s[lcur.si + 1] == '\n' || s[lcur.si + 1] == ' ' ||
            s[lcur.si + 1] == '\t')) {
      lcur = lcur.next(s[lcur.si]);
    }
    Logger logger;
    logger.print(cerr);
    cerr << "tokenizeBlock->2+Newline(" << lbegin << ")\n";

    return new T(TT::LineBreak, Span(lbegin, lcur));
  }
  if (s[lcur.si] == '\n') {
    Logger logger;
    logger.print(cerr);
    cerr << "tokenizeBlock->1Newline(" << lbegin << ")\n";
    return new T(TT::RawText, Span(lcur, lcur.nextline()));
  }
  if (strpeek(s + lcur.si, "$$")) {
    Logger logger;
    logger.print(cerr);
    cerr << "tokenizeBlock->LatexBlock(" << lbegin << ")\n";
    lcur = lcur.next("$$");

    // TODO: fix error message here, that will get generated from strconsume.
    // I had never thought about the problem that occurs when the opening
    // and closing braces are the same...
    lcur = strconsume(lcur, s, "$$", "unclosed $$ tag.");

    // we need to have $$\n
    if (lcur.si < len && s[lcur.si] != '\n') {
      printf_err_loc(lcur, s,
                "incorrectly terminated $$."
                "must have newline following.");
      assert(false && "incorrectly terminated $$");
    }

    if (lcur.line - lbegin.line > LONG_LATEX_BLOCK_SIZE) {
      printf_err_loc(
          lbegin, s,
          "WARNING: latex block is very long! Perhaps block is overflowing?");
      printf_err_loc(lcur, s, "latex block ends here");
      assert(false && "very large latex block");
    }
    return new T(TT::LatexBlock, Span(lbegin, lcur));
  } else if (strpeek(s + lcur.si, "<script")) {
    Logger logger;
    logger.print(cerr);
    cerr << "tokenizeBlock->Script(" << lbegin << ")\n";

    lcur = strconsume(lcur, s, "</script>", "unclosed <script> tag.");
    return new T(TT::HTML, Span(lbegin, lcur));
  } else if (strpeek(s + lcur.si, "<!--")) {
    Logger logger;
    logger.print(cerr);
    cerr << "tokenizeBlock->Comment(" << lbegin << ")\n";

    lcur = strconsume(lcur, s, "-->", "unclosed comment till end of file.");
    return new T(TT::Comment, Span(lbegin, lcur));
  } else if (strpeek(s + lbegin.si, "```")) {
    Logger logger;
    logger.print(cerr);
    cerr << "tokenizeBlock->Code(" << lbegin << ")\n";

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
      printf_err_loc(lbegin, s, "``` has too long a language name: |%s|", langname);
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
      printf_err_loc(lcur, s,
                "incorrectly terminated ```."
                "must have newline following ```.");
      assert(false && "incorrectly terminated code block.");
    }

    if (lcur.line - lbegin.line > LONG_CODE_BLOCK_SIZE) {
      printf_err_loc(
          lbegin, s,
          "WARNING: code block is very long! Perhaps block is overflowing?");
      // TODO: convert this to an assert.
      // assert(false && "very large code block");
    }

    return new TCodeBlock(Span(lbegin, lcur), langname);
  } else if (strpeek(s + lcur.si, "#")) {

    Logger logger;
    logger.print(cerr);
    cerr << "tokenizeBlock->Heading(" << lbegin << ")\n";

    // cerr << "HEADING" << lcur << "\n";
    int i = 0;
    for (; lcur.si < len && s[lcur.si] == '#'; lcur = lcur.next('#')) {
      i++;
    };
    T *t = tokenizeInlineLine(s, len, lcur);
    // cerr << "HEADING" << lcur << "\n";
    return new THeading(i, Span(lcur, t->span.end), t);
  } else if (s[lcur.si] == '-') {

    Logger logger;
    logger.print(cerr);
    cerr << "tokenizeBlock->List(" << lbegin << ")\n";

    vector<T *> toks;
    while (1) {
      T *t = tokenizeHyphenListItem(s, len, lcur);
      toks.push_back(t);
      lcur = t->span.end;
      assert(s[lcur.si] == '\n');
      lcur = lcur.next('\n');
      if (s[lcur.si] == '-') {
        continue;
      }
      break;
    }
    return new TList(lbegin, toks);
  } else if ((lcur.si == 0 || s[lcur.si - 1] == '\n') &&
             isNumberedListBegin(s, len, lcur)) {

    Logger logger;
    logger.print(cerr);
    cerr << "tokenizeBlock->NumberedList(" << lbegin << ")\n";

    vector<T *> toks;
    int curnum = 1;
    while (1) {
      T *t = tokenizeNumberedListItem(s, len, lcur, curnum++);
      toks.push_back(t);
      lcur = t->span.end;
      assert(s[lcur.si] == '\n');
      lcur = lcur.next('\n');
      if (isNumberedListBegin(s, len, lcur)) {
        continue;
      }
      break;
    }
    return new TListNumbered(lbegin, toks);
  } else if (s[lcur.si] == '>') {
    return tokenizeQuoteItem(s, len, lcur);
  } else {

    Logger logger;
    logger.print(cerr);
    cerr << "tokenizeBlock->Inline(" << lbegin << ")\n";

    // consume whitespace.
    while (s[lcur.si] == '\n' || s[lcur.si] == '\t' || s[lcur.si] == ' ') {
      lcur = lcur.next(s[lcur.si]);
    }
    // TODO: add paragraph here!
    return tokenizeInlineLine(s, len, lbegin);
  }
}

void tokenize(const char *s, const ll len, vector<T *> &ts) {
  Logger logger;
  Span span(LOC_FIRST, LOC_FIRST);
  while (span.end.si < len) {
    cerr << "\n";
    logger.print(cerr);
    cerr << "tokenize loop(" << span.end << ")\n";
    T *t = tokenizeBlock(s, len, span.end);
    assert(t != nullptr);
    ts.push_back(t);

    logger.print(cerr);
    cerr << "debug: " << *t << "\n";

    // we always have to make progress.
    assert(span.end.si != t->span.end.si);
    span = t->span;
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
    printf_err_loc(span.begin, raw_input, "%s", duk_to_string(katex_ctx, -1));
    duk_pop(katex_ctx);
    // reload katex context.
    katex_ctx = load_katex();
    return {false, nullptr};
    assert(false && "unable to compile katex");
  }
}

// given the inline object, convert it to text a link can see.
// Ie, for example, on seeing
// - [$A = B$](...) or **what I need**
//
// this will return:
// A = B or what I need
// So this strips off all "decoration" leaving only the text in place.
// TODO: Refactor LatexInline, CodeInline, Bold, Italic
// to be same struct.
void inlineTokenToPlaintext(const char *raw_input, const T *t, char *outs,
                            ll &outlen) {

  if (t->ty == TT::InlineGroup) {
    // cerr << *t << "\n";
    ll len = 0;
    for (T *item : ((TInlineGroup *)t)->items) {
      inlineTokenToPlaintext(raw_input, item, outs + len, len);
    };
    outlen = len + 1;
  } else if (t->ty == TT::CodeInline) {
    Span span = Span(t->span.begin.next("`"), t->span.end.prev("`"));
    strncpy(outs, raw_input + span.begin.si, span.nchars());
    outlen += span.nchars();
  } else if (t->ty == TT::LatexInline) {
    Span span = Span(t->span.begin.next("$"), t->span.end.prev("$"));
    strncpy(outs, raw_input + span.begin.si, span.nchars());
    outlen += span.nchars();
  } else if (t->ty == TT::RawText) {
    strncpy(outs, raw_input + t->span.begin.si, t->span.nchars());
    outlen += t->span.nchars();
  } else if (t->ty == TT::Comment) {
    return;
  } else if (t->ty == TT::Bold) {
    TBold *bold = (TBold *)t;
    inlineTokenToPlaintext(raw_input, bold->item, outs, outlen);
  } else if (t->ty == TT::Italic) {
    TItalic *italic = (TItalic *)t;
    inlineTokenToPlaintext(raw_input, italic->item, outs, outlen);
  } else if (t->ty == TT::Link) {
    TLink *link = (TLink *)t;
    inlineTokenToPlaintext(raw_input, link->text, outs, outlen);
  } else {
    printf_err_loc(t->span.end, raw_input, "unexpected token in heading!");
    assert(false && "unexpected token in heading.");
  }
}

// make a link according to github flavoured markdown convention for
// a heading.
// https://gist.github.com/asabaylus/3071099
// The code that creates the anchors is here:
// https://github.com/jch/html-pipeline/blob/master/lib/html/pipeline/toc_filter.rb
//
// It downcases the string
// remove anything that is not a letter, number, space or hyphen (see the source
// for how Unicode is handled) changes any space to a hyphen. If that is not
// unique, add "-1", "-2", "-3",... to make it unique
GIVE const char *mkHeadingURL(KEEP const char *raw_input,
                              KEEP THeading *heading) {
  const int BUFSIZE = (1 << 10);
  char plaintext[BUFSIZE];
  for (int i = 0; i < BUFSIZE; ++i)
    plaintext[i] = 0;
  ll ptlen = 0;
  inlineTokenToPlaintext(raw_input, heading->item, plaintext, ptlen);
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
    // convert uppercase -> lowercase
    // keep digits
    // convert space to hyphen
    // convert groups of hyphens into single hyphen
    // remove everything else.
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

bool toHTML(duk_context *katex_ctx, duk_context *prism_ctx,
            const char *raw_input, const T *const t, ll &outlen, char *outs) {
  assert(t != nullptr);
  switch (t->ty) {
  case TT::Undefined: {
    assert(false && "Should not have received undefined");
    return false;
  }
  case TT::Comment: {
    return true;
  }

  case TT::HTML:
  case TT::RawText: {
    strncpy(outs + outlen, raw_input + t->span.begin.si, t->span.nchars());
    outlen += t->span.nchars();
    outs[outlen] = ' '; outlen++;
    return true;
  }

  case TT::LineBreak: {
    outs[outlen] = ' '; outlen++;
    return true;
  }

  case TT::CodeBlock: {
    TCodeBlock *block = (TCodeBlock *)t;
    // TODO: escape HTML content.
    const char *open = "<pre><code>";
    const char *close = "</code></pre>";

    strcpy(outs + outlen, open);
    outlen += strlen(open);

    // we want to ignore the first 3 ``` and the last 3 ```
    const Span span =
        Span(t->span.begin.next("```").next(block->langname).next("\n"),
             t->span.end.prev("```"));
    char *code_html = pygmentize(prism_ctx, raw_input, block->langname, span);

    strcpy(outs + outlen, code_html);
    outlen += strlen(code_html);
    free(code_html);

    strcpy(outs + outlen, close);
    outlen += strlen(close);
    return true;
  }

  case TT::LatexInline:
  case TT::LatexBlock: {
    const Span span =
        t->ty == TT::LatexBlock
            ? Span(t->span.begin.next("$$"), t->span.end.prev("$$"))
            : Span(t->span.begin.next('$'), t->span.end.prev('$'));

    if (t->ty == TT::LatexBlock) {
      outlen += sprintf(outs + outlen, "<div class='latexblock'>");
    } else if (t->ty == TT::LatexInline) {
      outlen += sprintf(outs + outlen, "<span class='latexinline'>");
    } else {
      assert(false && "expected latex inline or latex block");
    }

    char *outcompile;
    bool success;
    tie(success, outcompile) = compileLatex(katex_ctx, raw_input, span,
                     t->ty == TT::LatexBlock ? LatexType::LatexTypeBlock
                                             : LatexType::LatexTypeInline);
    if (!success) {
        return false;
    }
    strcpy(outs + outlen, outcompile);
    outlen += strlen(outcompile);

    free(outcompile);

    if (t->ty == TT::LatexBlock) {
      outlen += sprintf(outs + outlen, "</div>");
    } else if (t->ty == TT::LatexInline) {
      outlen += sprintf(outs + outlen, "</span>");
    } else {
      assert(false && "expected latex inline or latex block");
    }
    return true;
  }

  case TT::List: {
    const char *openul = "<ul>";
    const char *closeul = "</ul>";

    const char *openli = "<li>";
    const char *closeli = "</li>";

    TList *tlist = (TList *)t;
    strcpy(outs + outlen, openul);
    outlen += strlen(openul);
    for (auto it : tlist->items) {
      strcpy(outs + outlen, openli);
      outlen += strlen(openli);
      toHTML(katex_ctx, prism_ctx, raw_input, it, outlen, outs);
      strcpy(outs + outlen, closeli);
      outlen += strlen(closeli);
    }
    strcpy(outs + outlen, closeul);
    outlen += strlen(closeul);
    return true;
  }

  case TT::TListNumbered: {
    const char *openul = "<ol>";
    const char *closeul = "</ol>";

    const char *openli = "<li>";
    const char *closeli = "</li>";

    TList *tlist = (TList *)t;
    strcpy(outs + outlen, openul);
    outlen += strlen(openul);
    for (auto it : tlist->items) {
      strcpy(outs + outlen, openli);
      outlen += strlen(openli);
      toHTML(katex_ctx, prism_ctx, raw_input, it, outlen, outs);
      strcpy(outs + outlen, closeli);
      outlen += strlen(closeli);
    }
    strcpy(outs + outlen, closeul);
    outlen += strlen(closeul);
    return true;
  }

  case TT::Link: {
    TLink *link = (TLink *)t;
    // toHTML(raw_input, temp_dir_path, link->text,  outlen, outs);
    outlen += sprintf(outs + outlen, "<a href=%s>", link->link);
    toHTML(katex_ctx, prism_ctx, raw_input, link->text, outlen, outs);
    outlen += sprintf(outs + outlen, "</a>");
    return true;
  }

  case TT::InlineGroup: {
    outlen += sprintf(outs + outlen, "<span class='centered'>");
    TInlineGroup *group = (TInlineGroup *)t;
    for (T *t : group->items) {
      toHTML(katex_ctx, prism_ctx, raw_input, t, outlen, outs);
    }
    return true;
  }

  case TT::CodeInline: {
    const char *open = "<code class='inline'>";
    const char *close = "</code>";
    strcpy(outs + outlen, open);
    outlen += strlen(open);

    const Span span = Span(t->span.begin.next("`"), t->span.end.prev("`"));

    strncpy(outs + outlen, raw_input + span.begin.si, span.nchars());
    outlen += span.nchars();

    strcpy(outs + outlen, close);
    outlen += strlen(close);
    return true;
  }

  case TT::Heading: {
    THeading *theading = (THeading *)t;

    // need the _raw text_. Hmm.
    const char *link = mkHeadingURL(raw_input, theading);
    // outlen += sprintf(outs + outlen, "<h%d id=%s>", theading->hnum, link);
    outlen += sprintf(outs + outlen, "<h%d>", min(4, 1 + theading->hnum));
    outlen +=
        sprintf(outs + outlen, "<a id=%s href='#%s'> %s </a>", link, link, "§");
    toHTML(katex_ctx, prism_ctx, raw_input, theading->item, outlen, outs);
    outlen += sprintf(outs + outlen, "</h%d>", min(4, 1 + theading->hnum));

    free((char *)link);
    return true;
  }

  case TT::Italic: {
    TItalic *tcur = (TItalic *)t;
    outlen += sprintf(outs + outlen, "<i>");
    toHTML(katex_ctx, prism_ctx, raw_input, tcur->item, outlen, outs);
    outlen += sprintf(outs + outlen, "</i>");
    return true;
  }

  case TT::Bold: {
    TBold *tcur = (TBold *)t;
    outlen += sprintf(outs + outlen, "<b>");
    toHTML(katex_ctx, prism_ctx, raw_input, tcur->item, outlen, outs);
    outlen += sprintf(outs + outlen, "</b>");
    return true;
  }

  case TT::Quote: {
    TQuote *tq = (TQuote *)t;

    outlen += sprintf(outs + outlen, "<blockquote>");
    for (auto it : tq->items) {
      toHTML(katex_ctx, prism_ctx, raw_input, it, outlen, outs);
    }
    outlen += sprintf(outs + outlen, "</blockquote>");
    return true;
  }
  default:
    assert(false && "unhandled token type in toHTML");
  };
  assert(false && "unreachabe");
}

// TUFTE
// <body vlink="#660000" text="#000000" link="#CC0000"
//  bgcolor="#FFFFF3" alink="#660000">
const char html_preamble[] =
    "<!DOCTYPE html>"
    "<meta charset='UTF-8'>"
    "<html>"
    "<head>"
    // ===RSS===
    "<link rel='alternate' type='application/rss+xml' href='feed.rss' title='" "A universe of sorts'" "/>"
    // ===KateX===
    "<link rel='stylesheet' href='/katex/katex.min.css'"
    "    "
    "integrity='sha384-AfEj0r4/OFrOo5t7NnNe46zW/tFgW6x/"
    "bCJG8FqQCEo3+Aro6EYUG4+cU+KJWu/X'"
    "    crossorigin='anonymous'>"
    "<!-- The loading of KaTeX is deferred to speed up page rendering -->"
    // ===Prismjs===
    "<link rel='stylesheet' href='/prism/prism.css'>"
    //
    // "<script defer src='katex/katex.min.js'"
    // "
    // integrity='sha384-g7c+Jr9ZivxKLnZTDUhnkOnsh30B4H0rpLUpJ4jAIKs4fnJI+sEnkvrMWph2EDg4'"
    // "    crossorigin='anonymous'></script>"
    // "<script>"
    // "    function on_katex_load() {"
    // "        const katex_opts = ["
    // "            {left: '$', right: '$', display: false},"
    // "            {left: '$$', right: '$$', display: true}"
    // "        ];"
    // "        renderMathInElement(document.body, katex_opts);"
    // "        let elemsInline =
    // document.getElementsByClassName('latexinline');"
    // //
    // https://stackoverflow.com/questions/39959563/render-math-with-katex-in-the-browser
    // "        for (var i = 0; i < elemsInline.length; i++)
    // {katex.render(elemsInline.item(i).textContent, elemsInline.item(i));}" "
    // let elemsBlock = document.getElementsByClassName('latexblock');" " for
    // (var i = 0; i < elemsInline.length; i++)
    // {katex.render(elemsBlock.item(i).textContent, elemsBlock.item(i),
    // {displayMode: true});}" "    }"
    // "</script>"
    // "<!-- To automatically render math in text elements, include the
    // auto-render extension: -->"
    // "<script defer src='katex/auto-render.min.js'"
    // "
    // integrity='sha384-mll67QQFJfxn0IYznZYonOWZ644AWYC+Pt2cHqMaRhXVrursRwvLnLaebdGIlYNa'"
    // "    crossorigin='anonymous'"
    // "    onload='on_katex_load();'></script>"
    // ===End KateX===
    "<title> A Universe of Sorts </title>"
    "<style>"
    "@font-face {font-family: 'Blog Mono'; src: "
    "url('/static/iosevka-fixed-extended.ttf');}"
    "@font-face {font-family: 'Blog Sans'; src: "
    "url('/static/Exo2-Regular.ttf');}"
    "@font-face {font-family: 'Blog Serif'; src: "
    "url('/static/Revans-Regular.ttf');}"
    // body
    "html { font-size: 100%; min-height: 100% }"
    "html,body { text-size-adjust: none; -webkit-text-size-adjust: none; "
    "-moz-text-size-adjust: none; -ms-text-size-adjust: none; } "
    "body {"
    // " background: linear-gradient(to right, #1565C0 0%, #1565C0 50%, #E91E63 50%,   #E91E63 99%); "
    // " background: linear-gradient(to right, #1565C0 1%, #EFEFEF 1%, #EFEFEF 99%, #E91E63 99%); "
    // " background: linear-gradient(to right, #1565C0 50%, #C2185B 50%);"
    " min-height: 100vh; " // https://stackoverflow.com/questions/3740722/min-height-does-not-work-with-body
    " line-height: 1.5em; "
    " color: #000000; " // tufte
    " font-family: serif; "
    " font-size: 12px; "
    " margin-top: 0px; " // by default, there is a margin.
    " max-width: 100%; overflow-x: hidden; }"
    "\n"
    "h1, h2, h3, h4, h5 { font-family: 'Blog Mono', monospace; }"
    "h1, h2 { column-span: all; }"
    // img as a block. width: 100% makes it responsive with no fiddling
    // https://stackoverflow.com/questions/15458650/make-an-image-responsive-the-simplest-way
    "img { display:block; width: 100%; max-width: 800px; height: auto }"
    // container
    ".container { overflow-x: auto; overflow-y: hidden; "
    "             margin-top: 0px; height: 100%; min-height: 100%;"
    "             max-width: 100%; "
    "             font-size: 18px; line-height: 1.5em; "
    "             column-count: auto; column-width: 25em; column-fill: auto; "
    "             padding-left: 30px; padding-right: 30px; background: #FFFFFF;"
    "}"
    "@media (max-width: 480px) { "
    "  .container { margin-left: 1%; margin-right: " "1%; }"
    "  body { font-size: 30px; } "
    " } "
    "@media (max-width: 1024px) { "
    " .container { margin-left: 1%; margin-right: " "1%; }"
    "  body { font-size: 30px; }"
    "}"
    // desktop
    "@media (min-width: 1024px) { .container { margin-left: 1%; margin-right: 1%; } }"
    // class for image <div>
    ".image { }"
    "\n"
    "a:hover { color: #1a73e8; text-decoration: underline;  }" // hover
    "\n"
    "a { color: #1a73e8; text-decoration: none; }" // unvisited; default
    "\n"
    "a:visited { color: #1a73e8; text-decoration: none; }" // vlink
    "\n"
    "a:active { color: #1a73e8; text-decoration: none; }" // alink
    "\n"
    // code blocks, latex blocks, quote blocks
    "\n"
    "blockquote { margin-left: 0px; margin-right: 0px; }"
    " pre, .latexblock, blockquote { border-left-color:#BBB;  "
    "border-left-style: solid;"
    "      border-left-width: 5px; }"
    "pre, blockquote { padding-left: 10px; }"
    "\n"
    // monospace font
    "pre { font-family: 'Blog Mono', monospace; font-size: 90%;  }"
    // pre: allow scrolling in x direction
    "pre {  overflow-x: auto; }"
    // padding and margin for blocks
    ".latexblock, blockquote, pre { margin-top: 10px; margin-bottom: 10px; "
    "padding-bottom: 5px; padding-top: 5px; background-color: #FFFFFF; }"
    // latexblock should have regular line height for correct rendering.
    ".latexblock { line-height: 1em }"
    // overflow: latex and code block
    "\n"
    // inline latex: force all text to be on same line.
    "pre, kbd, samp, tt{ font-family:'Blog Mono',monospace; }"
    ".inline { white-space: nowrap; background:#efefef; }"
    // ul's for some reason are padded, and they render their bullets *outside*
    // their area. Fix both:
    // https://stackoverflow.com/questions/13938975/how-to-remove-indentation-from-an-unordered-list-item/13939142
    "ul, ol { list-style-position: inside; padding-left: 0; }"
    "ul { list-style-type: disclosure-closed; }"
    // RESPONSIVE
    // " @media (max-width: 1000px) {"
    // "    .container { max-width: 100%; padding: 0; margin-left: 10%;
    // margin-right: 0px;" "                 padding-left: 0px; padding-right:
    // 0px; border: none; }"
    // "}"
    // end style
    "</style>"
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


const char html_postamble[] = "</container>"
                              "</body>"
                              "</html>";

static const ll MAX_OUTPUT_BUF_LEN = (ll)1e9L;

char raw_input[MAX_CHARS];

bool is_h1(const T *t) {
  if (t->ty != TT::Heading) {
    return false;
  }
  if (((THeading *)(t))->hnum != 1) {
    return false;
  }
  return true;
}

// returns number of characters written
// ix: index to start from. This will be the index after the
// TODO: unify the API style of writeTableOfContentsHTML and toHTML, to have
// both of them take a `const char *` at index `0`, and return a `long long`
// of length.
long long writeTableOfContentsHTML(duk_context *katex_ctx,
                                   duk_context *prism_ctx,
                                   const char *raw_input, const vector<T *> &ts,
                                   KEEP char *outs) {

  printf("===writing TOC===\n");
  ll outlen = 0;
  ll ix_h1 = 0;

  outlen += sprintf(outs + outlen, "<ol reversed>");
  while (ix_h1 != (ll)ts.size()) {
    while (ix_h1 < (ll)ts.size() && !is_h1(ts[ix_h1])) {
      ix_h1++;
    }
    if (ix_h1 == (ll)ts.size()) {
      break;
    }

    assert(is_h1(ts[ix_h1]));
    THeading *theading = (THeading *)ts[ix_h1];
    ix_h1++; // proceed to next heading

    const char *url = mkHeadingURL(raw_input, theading);
    printf("---writing heading |%lld: %s|---\n", ix_h1, url);
    // outlen += sprintf(outs + outlen, "<h%d id=%s>", theading->hnum, link);
    outlen += sprintf(outs + outlen, "<li><a href='%s.html'>", url);
    toHTML(katex_ctx, prism_ctx, raw_input, theading->item, outlen, outs);
    outlen += sprintf(outs + outlen, "</a></li>");
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
    } else {
      out.push_back(c);
    }
  }

  static void mkHeadingRSSTitle(const char *raw_input, T *t, std::string &out) {
    if (t->ty == TT::InlineGroup) {
      TInlineGroup *group = (TInlineGroup *)t;
      for (T *item : group->items) {
        mkHeadingRSSTitle(raw_input, item, out);
      }
    } else if (t->ty == TT::Heading) {
      THeading *heading = (THeading *)t;
      mkHeadingRSSTitle(raw_input, heading->item, out);
    } else if (t->ty == TT::Bold) {
      TBold *bold = (TBold *)t;
      mkHeadingRSSTitle(raw_input, bold->item, out);
    } else if (t->ty == TT::Italic) {
      TItalic *italic = (TItalic *)t;
      mkHeadingRSSTitle(raw_input, italic->item, out);
    } else if (t->ty == TT::Link) {
      TLink *link = (TLink *)t;
      mkHeadingRSSTitle(raw_input, link->text, out);
    } else if (t->ty == TT::CodeInline || t->ty == TT::LatexInline) {
      for (int i = t->span.begin.si + 1; i < t->span.end.si - 1; ++i) {
        writeEscapedCharacter(raw_input[i], out);
      }
    } else if (t->ty == TT::RawText) {
      for (int i = t->span.begin.si; i < t->span.end.si; ++i) {
        writeEscapedCharacter(raw_input[i], out);
      }
      // at the end of raw text, write a space.
      writeEscapedCharacter(' ', out);
    } else {
      printf_err_loc(t->span.begin, raw_input,
                "unknown type of token type in a heading: |%d|", t->ty);
      std::cerr << "unknown token of type: |" << t->ty << "|\n";
      assert(false && "unknown type of heading to convert in RSS title");
    }
  }
  // https://www.mnot.net/rss/tutorial/
  static void writeRSSFeed(KEEP FILE *frss, KEEP const char *raw_input,
                           const vector<T *> &ts) {
    assert(frss != nullptr);
    // https://www.mnot.net/rss/tutorial/
    fprintf(frss, "<?xml version=\"1.0\"?>\n");
    fprintf(frss, "<rss version=\"2.0\">\n");
    fprintf(frss, "<channel>\n");
    fprintf(frss, "<title>A universe of sorts</title>\n");
    fprintf(frss, "<link>https://bollu.github.io/</link>\n");
    fprintf(frss, "<description>%s</description>\n",
            CONFIG_WEBSITE_RSS_DESCRIPTION);

    for (ll ix_h1 = 0; ix_h1 < (int)ts.size(); ++ix_h1) {
      if (!is_h1(ts[ix_h1])) {
        continue;
      }
      assert(is_h1(ts[ix_h1]));

      // <item>
      // <title>News for September the Second</title>
      // <link>http://example.com/2002/09/01</link>
      // <description>other things happened today</description>
      // </item>

      THeading *theading = (THeading *)ts[ix_h1];
      // TODO: make this a useful string of text, not the raw fucking URL
      const char *url = mkHeadingURL(raw_input, theading);
      std::string title;
      mkHeadingRSSTitle(raw_input, theading, title);

      fprintf(frss, "  <item>\n");
      fprintf(frss, "    <title>%s</title>\n", title.c_str());
      // tell the aggregators that we are using RSS 2.0
      fprintf(frss, "    <guid>%s/%s.html</guid>\n",
              CONFIG_WEBSITE_URL_NO_TRAILING_SLASH, url);
      fprintf(frss, "    <link>%s/%s.html</link>\n",
              CONFIG_WEBSITE_URL_NO_TRAILING_SLASH, url);
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

  vector<T *> ts;
  tokenize(raw_input, nread, ts);
  cout << "===Done tokenizing; Emitting HTML...===\n";

  // index of the latest <h1> tag.
  ll ix_h1 = 0;

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

    for (int i = 0; i < ix_h1; ++i) {
      toHTML(katex_ctx, prism_ctx, raw_input, ts[i], outlen, index_html_buf);
    }

    // ===write out table of contents===
    outlen += writeTableOfContentsHTML(katex_ctx, prism_ctx, raw_input, ts,
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
    assert(ts[ix_start]->ty == TT::Heading);
    THeading *heading = (THeading *)ts[ix_start];
    assert(heading->hnum == 1);

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
    for (int i = ix_start; i < ix_h1; ++i) {
      success &= toHTML(katex_ctx, prism_ctx, raw_input, ts[i], outlen, outbuf);
    }

    if (!success) {
      fprintf(stdout, "===ERROR: compile [%s] failed. skipping. ", url);
      assert(false && "failed compilation");
      continue;
    }

    outlen += sprintf(outbuf + outlen, utterances_preamble);
    outlen += sprintf(outbuf + outlen, "%s", html_postamble);

    // [ix_start, ix_h1) contains the new article
    char html_path[1024];

    sprintf(html_path, "%s%s.html", OUTPUT_ROOT_DIR_TRAILING_SLASH, url);

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

  RSS::writeRSSFeed(frss, raw_input, ts);
  fclose(frss);

  return 0;
}
