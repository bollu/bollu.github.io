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
    ThreeBacktick = 'B',
    Backtick = '`',
    Star = '*',
    Underscore = '_',
    TwoUnderscore = 'U',
    OpenAngleBracket = '<',
    CloseAngleBracket = '>',
    Comment = 'C',
    RawText = 'T',
    Newline = '\n',
    Space = ' ',
    HTML = 'H',
    Undefined = 42,
};

// L for line
struct L {
    ll si, line, col;
    L(ll si, ll line, ll col) : si(si), line(line), col(col){};
    L nextcol() const { return L(si + 1, line, col + 1); }
    L nextline() const { return L(si + 1, line + 1, 0); }
    L next(char c) const {
        if (c == '\n') {
            return nextline();
        } else {
            return nextcol();
        }
    }
};

std::ostream &operator<<(std::ostream &o, const L &l) {
    return cout << l.line << ":" << l.col;
}

struct Span {
    L begin, end;
    Span(L begin, L end) : begin(begin), end(end){};
};

std::ostream &operator<<(std::ostream &o, const Span &s) {
    return cout << s.begin << "-" << s.end;
}

void vprintferr(L line, const char *s, const char *fmt, va_list args) {
    char *outstr = nullptr;
    vasprintf(&outstr, fmt, args);
    assert(outstr);

    fprintf(stderr, "%lld:%lld --- %s", line.line, line.col, outstr);
    fprintf(stderr, "\n");

    // find the previous newline character.
    int i = line.si; for(; i >= 1 && s[i-1] != '\n'; i--) {}
    for(; s[i] != '\0' && s[i] != '\n'; ++i) { 
        if (i == line.si) { fprintf(stderr, "⌷"); }
        fprintf(stderr, "%c", s[i]);
    }
    fprintf(stderr, "\n");

    free(outstr);
}

void printferr(L line, const char *s, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vprintferr(line, s, fmt, args);
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
        case TT::Comment: return o << "COMMENT";
        case TT::HTML: return o << "HTML";
        case TT::RawText: return o << "RAW";
        case TT::Newline: return o << "NEWLINE";
        case TT::Undefined: return o << "UNDEFINED";
        default: return o << "TY(" << (char)ty << ")";
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
bool isprelinerequired(char c) {
    return c == (char)TT::ListHyphen || c == (char)TT::Hash;
}


// return true if haystack starts with needle
bool strpeek(const char* haystack, const char* needle) {
    int i = 0;
    while(haystack[i] != '\0' && needle[i] != '\0' && haystack[i] == needle[i]) { i++; }
    return needle[i] == '\0';
}

L strconsume(L l, const char *haystack, const char *needle,
        const char *errfmt, ...)  {
    const L lbegin = l;
    while (haystack[l.si] != '\0' && 
           !strpeek(haystack + l.si, needle)) {
        l = l.next(haystack[l.si]);
    }

    if (haystack[l.si] == '\0') {
        va_list args;
        va_start(args, errfmt);
        vprintferr(lbegin, haystack, errfmt, args);
        va_end(args);
        assert(false && "unable to consume string.");
    } else {
        for(ll i = 0; i < (ll)strlen(needle)-1; ++i) { l = l.next(haystack[l.si]); }
    }
    return l;
}

T tokenize(const char *s, const ll len, const L lbegin) {
    assert(lbegin.si < len);
    L lcur = lbegin;

    if (s[lcur.si] == '\t') {
        assert(false && "tabs are not allowed");
    }

    if (s[lcur.si] == ' ') {
        while (s[lcur.si] == ' ') {
            lcur = lcur.nextcol();
        }
        return T(TT::Space, Span(lbegin, lcur));
    }
    else if (strpeek(s + lcur.si, "\n-")) {
        lcur = strconsume(lcur, s, "\n-", "unable to consume NEWLINE-");
        return T(TT::ListHyphen, Span(lbegin, lcur));
    }
    // this kills off newlines.
    else if (s[lcur.si] == '\n') {
        return T(TT::Newline, Span(lbegin, lcur.nextline()));
    }
    // raw text.
    else if (s[lcur.si] == '[') {
        return T(TT::OpenSquare, Span(lbegin, lcur.nextcol()));
    }
    else if (s[lcur.si] == ']') {
        return T(TT::CloseSquare, Span(lbegin, lcur.nextcol()));
    }
    else if (s[lcur.si] == '(') {
        return T(TT::OpenRound, Span(lbegin, lcur.nextcol()));
    }
    else if (s[lcur.si] == ')') {
        return T(TT::CloseRound, Span(lbegin, lcur.nextcol()));
    
    }
    else if (strpeek(s + lcur.si, "<script")) {
        lcur = strconsume(lcur, s, "</script>", "unclosed <script> tag.");
        return T(TT::HTML, Span(lbegin, lcur.nextcol()));
    }
    else if (strpeek(s + lcur.si, "<!--")) {
        lcur = strconsume(lcur, s, "-->", "unclosed comment till end of file.");
        return T(TT::Comment, Span(lbegin, lcur.nextcol()));
    }
    else {
        // consume till a newline, or till a special char.
        while (s[lcur.si] != '*' && 
                s[lcur.si] != '[' && 
                s[lcur.si] != '<' && 
                s[lcur.si] != '<') { 

            // if we have a newline and then a "newline based" token, quit.
            if (s[lcur.si] == '\n' && 
                isprelinerequired(s[lcur.si+1])) {
                return T(TT::RawText, Span(lbegin, lcur));
            }

            lcur = lcur.next(s[lcur.si]);
        }
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
    Span span(L(0, 1, 0), L(0, 0, 0));
    while (span.end.si < len) {
        const T t = tokenize(s, len, span.begin);
        cout << "token: " << t << "\n";
        span = Span(t.span.end, t.span.end);
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
