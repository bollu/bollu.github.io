// https://spec.commonmark.org/0.29/#preliminaries
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <string>
#include <stdarg.h>
#include "utf8.h"
using ll = long long;
static const ll PADDING = 10;
static const ll MAX_TOKENS = 1e7;
static const ll MAX_CHARS = 1e7;

// Markdown grammar:
//

void asserterr(bool b, const char *str, ...) {
    if (b) return;
    // va_list ap;
    // printf(str, ap);
    assert(false && "assertion failed");
};

using namespace std;

// Error types
enum class ErrT {};

// Error
struct Err {};

// Expressinon
enum class ET {};

// Xpression.
struct E {
    ET type;
    virtual ostream &print(ostream &o) const = 0;
};

struct ERawText : public E {};
struct EBlockCode {};
struct EList {};
struct EInlineMath {};
struct EInlineCode {};
struct EBlockMath {};
struct EHeading {};
struct EHref : public E {
    E *text, *link;

   public:
    EHref(E *text, E *link) : text(text), link(link){};
};
struct EImage {};
struct EBold {};
struct EItalic {};

enum class TT {
    OpenSquare = '[',
    CloseSquare = ']',
    OpenRound = '(',
    CloseRound = ')',
    Hash = '#',
    ListHyphen = '-',
    DoubleQuote = '"',
    Backtick = '`',
    Star = '*',
    Underscore = '_',
    OpenAngleBracket = '<',
    CloseAngleBracket = '>',
    Newline = '\n',
    Space = ' ',
    Dollar = '$',
    ThreeBacktick = 300,
    TwoUnderscore,
    DoubleDollar,
    Comment,
    RawText,
    HTML,
    LatexBlock,
    LatexInline,
    CodeInline,
    CodeBlock,
    Undefined,
};

// L for line
struct L {
    ll si, line, col;
    L(ll si, ll line, ll col) : si(si), line(line), col(col){};
    L nextcol() const { return L(si + 1, line, col + 1); }
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
        for(int i = 0; s[i] != 0; ++i) { l = l.next(s[i]); }
        return l;
    }
};
const L firstline = L(0, 1, 1);

std::ostream &operator<<(std::ostream &o, const L &l) {
    return cout << ":" << l.line << ":" << l.col;
}

struct Span {
    L begin, end;
    Span(L begin, L end) : begin(begin), end(end){};
};

std::ostream &operator<<(std::ostream &o, const Span &s) {
    return cout << s.begin << " - " << s.end;
}

void vprintferr(L line, const char *filestr, const char *fmt, va_list args) {
    char *outstr = nullptr;
    vasprintf(&outstr, fmt, args);
    assert(outstr);

    cerr << line << " --- " << outstr << "\n";
    // find the previous newline character.
    int i = line.si; for(; i >= 1 && filestr[i-1] != '\n'; i--) {}
    for(; filestr[i] != '\0' && filestr[i] != '\n'; ++i) { 
        if (i == line.si) { cerr <<  "⌷"; } cerr << filestr[i];
    }
    cerr << "\n";
    free(outstr);
}

void printferr(L line, const char *filestr, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vprintferr(line, filestr, fmt, args);
    va_end(args);
}

// T type, optional string data.
struct T {
    TT ty = TT::Undefined;
    Span span = Span(L(-1, -1, -1), L(-1, -1, -1));

    T(TT ty, Span span) : ty(ty), span(span){};
    T(){};
};

std::ostream &operator<<(std::ostream &o, const TT &ty) {
    switch (ty) {
        case TT::ThreeBacktick: return o << "```";
        case TT::TwoUnderscore: return o << "__";
        case TT::DoubleDollar: return o << "$$";
        case TT::Comment: return o << "COMMENT";
        case TT::HTML: return o << "HTML";
        case TT::LatexBlock: return o << "LatexBlock";
        case TT::LatexInline: return o << "LatexInline";
        case TT::CodeBlock: return o << "CodeBlock";
        case TT::CodeInline: return o << "CodeInline";
        case TT::RawText: return o << "RAW";
        case TT::Newline: return o << "NEWLINE";
        case TT::Undefined: return o << "UNDEFINED";
        default: assert((int) ty <= 128); return o << "TY(" << (char)ty << ")";
    }
};

std::ostream &operator<<(std::ostream &o, const T &t) {
    return cout << t.ty << "[" << t.span << "]";
}

bool issinglechartoken(char c) {
    return c == (char)TT::OpenSquare || c == (char)TT::CloseSquare ||
           c == (char)TT::OpenRound || c == (char)TT::CloseRound ||
           c == (char)TT::Hash || c == (char)TT::ListHyphen ||
           c == (char)TT::DoubleQuote || c == (char)TT::Backtick ||
           c == (char)TT::Star || c == (char)TT::Underscore;
};


// do these need a newline before them to be parsed as token? eg. 
// - is treated as a list item
// # is treated as a heading.
bool is_char_post_newline(char c) {
    return c == (char)TT::ListHyphen || c == (char)TT::Hash;
}

bool is_char_special_token(char c) {
        return  c == '*' || c == '[' || c == '<' || c == '>' || c == '$' || c == '`';
}


// return true if haystack starts with needle
bool strpeek(const char* haystack, const char* needle) {
    int i = 0;
    while(haystack[i] != '\0' && needle[i] != '\0' && haystack[i] == needle[i]) { i++; }
    return needle[i] == '\0';
}

// consume till we file delim in filestr
L strconsume(L l, const char *filestr, const char *delim,
        const char *errfmt, ...)  {
    const L lbegin = l;
    while (filestr[l.si] != '\0' && 
           !strpeek(filestr + l.si, delim)) {
        l = l.next(filestr[l.si]);
    }

    if (filestr[l.si] == '\0') {
        va_list args;
        va_start(args, errfmt);
        vprintferr(lbegin, filestr, errfmt, args);
        va_end(args);
        assert(false && "unable to consume string.");
    } else {
        for(ll i = 0; i < (ll)strlen(delim)-1; ++i) { l = l.next(filestr[l.si]); }
    }
    return l;
}

// TODO: convert \vert into |
// TODO: preprocess and check that we don't have \t tokens anywhere.
T tokenize(const char *s, const ll len, const L lbegin) {
    assert(lbegin.si < len);
    L lcur = lbegin;

    if (strpeek(s + lcur.si, "$$")) {
        lcur = lcur.next("$$");
        // TODO: fix error message here, that will get generated from strconsume.
        // I had never thought about the problem that occurs when the opening
        // and closing braces are the same...
        lcur = strconsume(lcur, s, "$$", "unclosed $$ tag.");
        return T(TT::LatexBlock, Span(lbegin, lcur.nextcol()));
    } else if (strpeek(s + lcur.si, "<script")) {
        lcur = strconsume(lcur, s, "</script>", "unclosed <script> tag.");
        return T(TT::HTML, Span(lbegin, lcur.nextcol()));
    } else if (strpeek(s + lcur.si, "<!--")) {
        lcur = strconsume(lcur, s, "-->", "unclosed comment till end of file.");
        return T(TT::Comment, Span(lbegin, lcur.nextcol()));
    } else if (strpeek(s + lcur.si, "$$")) {
        // consume everything till end as latex.
        lcur = strconsume(lcur, s, "-->", "unclosed comment till end of file.");
        return T(TT::Comment, Span(lbegin, lcur.nextcol()));
    } else if (strpeek(s + lcur.si, "```")) {
        lcur = lcur.next("```");
        lcur = strconsume(lcur, s, "```", "unclosed code block tag.");
        return T(TT::CodeBlock, Span(lbegin, lcur.nextcol()));
    } else if (s[lcur.si] == '\n') { // this kills off newlines.
        return T(TT::Newline, Span(lbegin, lcur.nextline()));
    } else if (s[lcur.si] == '-') {
        return T(TT::ListHyphen, Span(lbegin, lcur.nextcol()));
    } else if (s[lcur.si] == '[') {
        return T(TT::OpenSquare, Span(lbegin, lcur.nextcol()));
    } else if (s[lcur.si] == ']') {
        return T(TT::CloseSquare, Span(lbegin, lcur.nextcol()));
    } else if (s[lcur.si] == '(') {
        return T(TT::OpenRound, Span(lbegin, lcur.nextcol()));
    } else if (s[lcur.si] == ')') {
        return T(TT::CloseSquare, Span(lbegin, lcur.nextcol()));
    } else if (s[lcur.si] == '`') {
        lcur = lcur.nextcol();
        // TODO: fix error message here. 
        lcur = strconsume(lcur, s, "`", "unclosed inline code block `...`");

        if (lbegin.line != lcur.line) {
            printferr(lbegin, s, "inline code block `...` not allowed to be on two different lines."); 
            assert(false && "inline code block `...` on two different lines.");
        }

        return T(TT::CodeInline, Span(lbegin, lcur.nextcol()));

    } else if (s[lcur.si] == '$') { // order is important; this should come here, after $$ has been tried.
        lcur = lcur.nextcol();
        // TODO: fix error message here. 
        lcur = strconsume(lcur, s, "$", "unclosed inline latex block $");

        if (lbegin.line != lcur.line) {
            printferr(lbegin, s, "inline latex block not allowed to be on two different lines."); 
            assert(false && "inline latex block on two different lines.");
        }

        return T(TT::LatexInline, Span(lbegin, lcur.nextcol()));
    }
    else if (s[lcur.si] == ' ') { 
        while (s[lcur.si] == ' ') { lcur = lcur.nextcol(); }
        return T(TT::Space, Span(lbegin, lcur));
    } else {
        // consume till a newline, or till a special char. If it _were_
        // part of a special form as the _first_ character, it would
        // have been consumed already. Hence, a do-while loop.
        do { 
            // if we have a newline and then a "newline based" token, quit.
            if (s[lcur.si] == '\n' && 
                is_char_post_newline(s[lcur.si+1])) {
                return T(TT::RawText, Span(lbegin, lcur));
            } else {
                lcur = lcur.next(s[lcur.si]);
            }
        } while(!is_char_special_token(s[lcur.si]) && s[lcur.si] != '\0');
        return T(TT::RawText, Span(lbegin, lcur));

    }

    printferr(lcur, s, "unknown begin character: |%c|", s[lcur.si]);
    assert(false && "unreachable");
}

// peek into the token stream without consuming.
/*
TT peek(const char *s, const int len, int si) {
    return tokenize(s, len, si).ty;
};


void expect(const char *s, const int len, int &si, TT ty) {
    T tok = tokenize(s, len, si);
    assert(tok.ty == ty);
}
*/

E *parse(const char *s, const ll len, ll &si) {
    Span span(firstline, firstline);
    while (span.end.si < len) {
        const L prevl = span.begin;
        const T t = tokenize(s, len, span.begin);
        cout << "token: " << t << "\n";
        span = Span(t.span.end, t.span.end);
        // we always have to make progress.
        assert(prevl.si != t.span.end.si);
    }
    return nullptr;
}

T ts[MAX_TOKENS];
char str[MAX_CHARS];
int main(int argc, char **argv) {
    if (argc != 2) {
        printf("expected usage: %s <path>", argv[0]);
        return 1;
    }
    FILE *f = fopen(argv[1], "r");
    if (f == nullptr) {
        printf("unable to open file: |%s|\n", argv[1]);
        return -1;
    }

    fseek(f, 0, SEEK_END);
    const ll len = ftell(f);
    fseek(f, 0, SEEK_SET);
    assert(len < MAX_CHARS);

    cout << "len: " << len << "\n";

    const ll nread = fread(str, 1, len, f);
    assert(nread == len);

    ll si = 0;
    E *e = parse(str, nread, si);
    e->print(cerr);

    return 0;
}
