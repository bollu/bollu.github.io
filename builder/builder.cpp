// https://spec.commonmark.org/0.29/#preliminaries
// TODO: RSS feed.
// Font to try: Iosevka
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <string>
#include <stdarg.h>
#include <vector>
#include <utility>
#include <unordered_map>
#include <tuple>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "utf8.h"

#define GIVE
#define TAKE
#define KEEP


using namespace std;

using ll = long long;
static const ll PADDING = 10;
static const ll MAX_TOKENS = 1e7;
static const ll MAX_CHARS = 1e7;

struct Options {
    bool latex2ascii = false; // convert latex to ascii
} G_OPTIONS;


const char DB_PATH[]="./blogcache.txt";
unordered_map<ll, const char *> G_DB;
void loadDB() {
    G_DB = {};
    FILE *f = fopen(DB_PATH, "rb");
    if (f == nullptr) { 
        cerr << __FUNCTION__ << ": WARNING: no DB file found at |" << DB_PATH << "|\n";
        return;
    }
    while (!feof(f)) {
        ll k, len;
        fread(&k, sizeof(ll), 1, f);
        if (feof(f)) break;

        fread(&len, sizeof(ll), 1, f);
        cerr << __FUNCTION__ << ": loading DB[" << k << "] (size: " << len << ")\n";
        char *buf = (char *)calloc(sizeof(char), len + 2); //(char *)calloc(len + 2);
        fread(buf, sizeof(char), len, f);

        assert(!G_DB.count(k) && "key already in DB");
        G_DB.insert(make_pair(k, buf));
        cerr << __FUNCTION__ << ": DB[" << k << "] := " << buf << "\n";
    }
    cerr << __FUNCTION__ << ": done reading file.\n";
    fclose(f);
};

// TODO: make a combined error reporting and assertion function.
const char *lookup_key(ll k) {
    unordered_map<ll, const char *>::iterator it = G_DB.find(k);
    if (it == G_DB.end()) { return nullptr; }
    return it->second;
};

void store_key_value(const ll k, KEEP const char *v, const ll len) {
    assert(G_DB.count(k) == 0);
    G_DB.insert(make_pair(k, strdup(v)));

    // TODO: cache this;
    FILE *f = fopen(DB_PATH, "ab");
    assert(f != nullptr && "unable to open DB file");
    fwrite(&k, sizeof(ll), 1, f);
    fwrite(&len, sizeof(ll), 1, f);
    fwrite(v, sizeof(char), len, f);
    fclose(f);
}

ll hashstr(const char *s, const ll len) {
    const ll p = 53;
    // closest prime below 2^62. found using b(2^62) on 
    // https://www.alpertron.com.ar/ECM.HTM
    const ll mod = 1e9 + 9;
    ll h = 0;
    ll ppow = 1;
    for (int i = 0; i < len; ++i) {
        assert(s[i] != '\0');
        h +=  ((s[i] + 1) * ppow) % mod;
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
        case TT::Comment: return o << "COMMENT";
        case TT::HTML: return o << "HTML";
        case TT::LineBreak: return o << "LineBreak";
        case TT::LatexBlock: return o << "LatexBlock";
        case TT::LatexInline: return o << "LatexInline";
        case TT::CodeBlock: return o << "CodeBlock";
        case TT::CodeInline: return o << "CodeInline";
        case TT::RawText: return o << "RAW";
        case TT::Undefined: return o << "UNDEFINED";
        case TT::Link: return o << "LINK";
        case TT::List: return o << "LIST";
        case TT::TListNumbered: return o << "LISTNUMBERED";
        case TT::InlineGroup: return o << "INLINEGROUP";
        case TT::Heading: return o << "HEADING";
        case TT::Italic: return o << "ITALIC";
        case TT::Bold: return o << "BOLD";
        case TT::Quote: return o << "Quote";
    }
    assert(false && "unreachable");
};

// L for line
struct L {
    ll si, line, col;
    L(ll si, ll line, ll col) : si(si), line(line), col(col){ };
    L nextcol() const { return L(si + 1, line, col + 1); }
    L prevcol() const { assert(col-1 >= 1); return L(si - 1, line, col - 1); }
    L nextline() const { return L(si + 1, line + 1, 1); }
    L next(char c) const {
        if (c == '\n') { return nextline(); } else { return nextcol(); }
    }

    L next(const char *s) const {
        L l = *this;
        for(int i = 0; s[i] != 0; ++i) { l = l.next(s[i]); } return l;
    }

    L prev(char c) {
        if (c == '\n') { assert(false && "don't know how to walk back newline");
        } else { return prevcol(); }
    }

    L prev(const char *s) const {
        L l = *this;
        for(int i = strlen(s)-1; i >= 0; --i) { l = l.prev(s[i]); } return l;
    }


    bool operator == (const L &other) const {
        return si == other.si && line == other.line && col == other.col;
    } 

    bool operator != (const L &other) const {   return !(*this == other); }
};
const L lfirstline = L(0, 1, 1);
const L lundefined = L(-1, -1, -1);

std::ostream &operator<<(std::ostream &o, const L &l) {
    return cout << ":" << l.line << ":" << l.col;
}

// half open [...)
// substr := str[span.begin...span.end-1];
struct Span {
    L begin, end;
    Span(L begin, L end) : begin(begin), end(end){ assert(end.si >= begin.si); };
    ll nchars() const { return end.si - begin.si; }
};

std::ostream &operator<<(std::ostream &o, const Span &s) {
    return cout << s.begin << " - " << s.end;
}

void vprintferr(L line, const char *instr, const char *fmt, va_list args) {
    char *outstr = nullptr;
    vasprintf(&outstr, fmt, args);
    assert(outstr);

    cerr << line << " --- " << outstr << "\n";
    // find the previous newline character.
    int i = line.si; for(; i >= 1 && instr[i-1] != '\n'; i--) {}

    cerr << "> ";
    for(; instr[i] != '\0' && instr[i] != '\n'; ++i) { 
        if (i == line.si) { cerr <<  "⌷"; } cerr << instr[i];
    }
    cerr << "\n";
    free(outstr);
}

void printferr(L line, const char *instr, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vprintferr(line, instr, fmt, args);
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
    return cout << t.ty << "[" << t.span << "]";
}

// TLink for link information.
struct TLink : public T {
    // link is taken ownership of.
    TLink(Span span, T *text, const char *link) : T(TT::Link, span), text(text), link(link) {};
    T *text;
    const char *link;
};

Span mkTokensSpan(const vector<T*> &items) {
    assert(items.size() > 0);
    Span s = items[0]->span;
    for(int i = 1; i < (int)items.size(); ++i) { 
        Span next = items[i]->span;

        if (next.begin.si < s.end.si) {
            cerr << "merging: " << *items[i-1] << " | " << *items[i] << "\n";
        }
        assert(next.begin.si >= s.end.si);
        s = Span(s.begin, next.end);
    }
    return s;
}

struct TList : public T {
    vector<T*> items;
    TList(vector<T*> items) : T(TT::List, mkTokensSpan(items)), items(items) { };
};

struct TListNumbered : public T {
    vector<T*> items;
    TListNumbered(vector<T*> items) : T(TT::TListNumbered, mkTokensSpan(items)), items(items) { };
};

struct TCodeBlock : public T {
    TCodeBlock(Span span, KEEP const char *langname) : 
        T(TT::CodeBlock, span), langname(strdup(langname)) {}
    char *langname;
};

struct TInlineGroup : public T {
    TInlineGroup(vector<T*> items) : 
        T(TT::InlineGroup, mkTokensSpan(items)), items(items) {} 
    vector<T*> items;
};

struct THeading : public T {
    THeading(int hnum, Span span, T*item) : T(TT::Heading, span),
        hnum(hnum), item(item) {};
    int hnum;
    T *item;
};

struct TItalic : public T {
    TItalic(Span span, T *item) : T(TT::Italic, span), item(item) {};
    T *item;
};

struct TBold : public T {
    TBold(Span span, T *item) : T(TT::Bold, span), item(item) {};
    T *item;
};

struct TQuote : public T {
    TQuote(Span span, vector<T*> items) : T(TT::Quote, span), items(items) {};
    vector<T*> items;
};


bool is_char_special_inline_token(char c) {
        return  c == '*' || c == '[' || c == ']' || c == '<' || c == '>' || c == '$' ||
        c == '`' || c == '\n' || c == '_';
}


// return true if haystack starts with needle
bool strpeek(const char* haystack, const char* needle) {
    int i = 0;
    while(haystack[i] != '\0' && needle[i] != '\0' && haystack[i] == needle[i]) { i++; }
    return needle[i] == '\0';
}


// consume till we file delim in instr
L strconsume(L l, const char *instr, const char *delim,
        const char *errfmt, ...)  {
    const L lbegin = l;
    while (instr[l.si] != '\0' && 
           !strpeek(instr + l.si, delim)) {
        l = l.next(instr[l.si]);
    }

    if (instr[l.si] == '\0') {
        va_list args;
        va_start(args, errfmt);
        vprintferr(lbegin, instr, errfmt, args);
        va_end(args);
        assert(false && "unable to consume string.");
    } else {
        assert(strpeek(instr + l.si, delim));
        l = l.next(delim);
    }
    return l;
}

T *tokenizeLink(const char *s, const ll len, const L opensq);
T *tokenizeNext(const char *s, const ll len, const L lbegin);

template<typename Fn>
T *tokenizeInlineTill(const char *s, const ll len, const L lbegin, Fn should_break) {
    vector<T *> ts;
    L lcur = lbegin;
    while(lcur.si < len) {
        T *t = tokenizeNext(s, len, lcur);
        ts.push_back(t);
        lcur = t->span.end;
        if (should_break(s, lcur)) { break; }
    }

    return new TInlineGroup(ts);
}

// tokenize that data that can come in an inline region. So this is:
// - raw text
// - inline math
// - inline code block
// - links (if allowed)
// TODO: add error checking for ``` for $$.
T *tokenizeNext(const char *s, const ll len, const L lbegin) {
    assert(lbegin.si < len);
    L lcur = lbegin;

    T *linkt = nullptr;
    if (s[lcur.si] == '[' && 
            (linkt = tokenizeLink(s, len, lcur)) != nullptr) {
        lcur = linkt->span.end;
        return linkt;
    }
    else if (s[lcur.si] == '`') {
        lcur = lcur.nextcol();
        // TODO: fix error message here. 
        lcur = strconsume(lcur, s, "`", "unclosed inline code block `...`");

        if (lbegin.line != lcur.line) {
            printferr(lbegin, s, "inline code block `...` not allowed to"
                    "be on two different lines."); 
            assert(false && "inline code block `...` on two different lines.");
        }

        return new T(TT::CodeInline, Span(lbegin, lcur));
    }
    else if (s[lcur.si] == '$') {
        lcur = lcur.nextcol();
        // TODO: fix error message here. 
        lcur = strconsume(lcur, s, "$", "unclosed inline latex block $");

        if (lbegin.line != lcur.line) {
            printferr(lbegin, s, "inline latex block not allowed to be on two different lines."); 
            assert(false && "inline latex block on two different lines.");
        }

        return new T(TT::LatexInline, Span(lbegin, lcur));
    }
    else if (lcur.si < len - 1 &&
             (s[lcur.si] == '*' || s[lcur.si] == '_') &&
             s[lcur.si+1] == s[lcur.si]) {
        const char c = s[lcur.si];
        const char delim[3] = { c, c, 0 };

        cerr << "BOLD " << lcur << "\n";
        T *item = tokenizeInlineTill(s, len, lcur.next(delim), [delim](const char *s, L lcur) {
                return strpeek(s + lcur.si, delim) || s[lcur.si] == '\n';
            });

        lcur = item->span.end;
        if (lcur.si == len) {
            printferr(lbegin, s, "unmatched bold demiliter: |%s|.", delim);
            assert(false && "unmatched bold delimiter");
        }

        assert(lcur.si < len); 

        if (s[lcur.si] == '\n') {
            printferr(lbegin, s, "bold emphasis spread across multiple lines!");
            assert(false && "bold spread across multiple lines");
        }

        assert(strpeek(s + lcur.si, delim)); lcur = lcur.next(delim);
        return new TBold(Span(lbegin, lcur), item);
    }
    //This ordering is important; bold MUST be checked first.
    else if (s[lcur.si] == '*' || s[lcur.si] == '_') {
        const char c = s[lcur.si];
        T *item = tokenizeInlineTill(s, len, lcur.nextcol(), [c](const char *s, L lcur) {
                return s[lcur.si] == c || s[lcur.si] == '\n';
        });

        lcur = item->span.end;
        if (lcur.si == len) {
            printferr(lbegin, s, "unmatched italic demiliter: |%c|.", c);
        }
        assert(lcur.si < len); 

        if (s[lcur.si] == '\n') {
            printferr(lbegin, s, "italic emphasis spread across multiple lines!");
            assert(false && "italic spread across multiple lines");
        }
        assert(s[lcur.si] == c);
        return new TItalic(Span(lbegin, lcur.nextcol()), item);

    }
    else {
        do { 
            lcur = lcur.next(s[lcur.si]);
        } while(!is_char_special_inline_token(s[lcur.si]) && s[lcur.si] != '\0' && lcur.si < len);
        return new T(TT::RawText, Span(lbegin, lcur));
    }
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
    assert(opensq.si < len);
    assert(s[opensq.si] == '[');

    T *text = tokenizeInlineTill(s, len, opensq.nextcol(), 
            [](const char *s, L lcur) { return s[lcur.si] == ']'; });

    const L closesq = text->span.end;
    if (closesq.si >= len) { return nullptr; }
    assert(s[closesq.si] == ']');
    
    const L openround = closesq.next(s[closesq.si]);
    if(s[openround.si] != '(') { return nullptr; };

    L closeround = openround;
    while(s[closeround.si] != ')' && s[closeround.si] != '\0')  { 
        closeround = closeround.next(s[closeround.si]);
    }
    if (s[closeround.si] != ')') { return nullptr; }

    char *link = (char *)calloc((closeround.si - openround.si), sizeof(char));
    for(int i = 0, j = openround.si+1; s[j] != ')'; ++i, ++j) {
        link[i] = s[j];
    }
    
    // TODO: change `tokenize` to `tokenizeInline` when the time is right.
    // std::tie(newline, text) = tokenize(s, closesq.si - opensq.si, opensq, false);
    // text = 
    return new TLink(Span(opensq, closeround.nextcol()), text, link);
}

// we are assuming that this is called on the *first* list item that
// has been seen.
T* tokenizeHyphenListItem (const char *s, const ll len, const L lhyphen) {
    assert(s[lhyphen.si] == '-');
    const L ltextbegin = lhyphen.nextcol();
    return tokenizeInlineTill(s, len, ltextbegin, [lhyphen, len](const char *s, L lcur) {
            if (s[lcur.si] != '\n') { return false; }
            assert(s[lcur.si] == '\n');
            if (lcur.si + 1 >= len) { return true; }
            // file is still left. Check that what's going on next is legit..
            // TODO: consider moving this logic to be _ouside_ here.
            // should probably be in the caller of tokenizeHyphenListItem.
            if (s[lcur.si + 1] == '-') { return true; }
            if (s[lcur.si + 1] == '\n') { return true; }
            // do NOT quit, since we are trying to *continue* this item;
            if (s[lcur.si + 1] == ' ') { return false; }
            printferr(lhyphen, s, 
                    "list item must have:"
                    "\n - two newline separation"
                    "\n - a new list item"
                    "\n - continuation, hanging under the previous text");
            assert(false && "list item ended improperly");
    });
}

// we are assuming that this is called on the *first* list item that
// has been seen.
T* tokenizeNumberedListItem (const char *s, const ll len, const L lhyphen,
        const ll curnum) {

    char curitem[7]; sprintf(curitem, "%lld.", curnum);
    if(!strpeek(s + lhyphen.si, curitem)) {
        printferr(lhyphen, s,
                "Expected list item to start with number: |%lld|", curnum);
        assert(false && "list item not respecting numbering.");
    }

    const L ltextbegin = lhyphen.next(curitem);
    return tokenizeInlineTill(s, len, ltextbegin, [len](const char *s, L lcur) {
            if (s[lcur.si] != '\n') { return false; }
            assert(s[lcur.si] == '\n');
            if (lcur.si + 1 >= len) { return true; }
            // do not quit, since we are trying to contine this item.
            if (s[lcur.si + 1] == ' ') { return false; }
            // otherwise, quit list item.
            return true;
    });
}


// We parse quotes here.
T* tokenizeQuoteItem (const char *s, const ll len, const L lquote) {
    assert(s[lquote.si] == '>');
    const L ltextbegin = lquote.nextcol();
    return tokenizeInlineTill(s, len, ltextbegin, [len, lquote](const char *s, L lcur) {
            if (s[lcur.si] != '\n') { return false; }
            assert(s[lcur.si] == '\n');
            if (lcur.si + 1 >= len) { return true; }
            // file is still left. Check that what's going on next is legit..
            // TODO: consider moving this logic to be _ouside_ here.
            // should probably be in the caller of tokenizeHyphenListItem.
            if (s[lcur.si + 1] == '>') { return true; }
            if (s[lcur.si + 1] == '\n') { return true; }
            printferr(lquote, s, 
                    "quote item must have:"
                    "\n- two newline separation"
                    "\n- quote continuation: '> ...'");
            assert(false && "list item ended improperly");
    });
}

// return if s[lbegin...] = <number>"."
// eg. 
// 1.
// 2.
// ... 10.
// NOTE: this does NOT check that it is at the beginning of a new line.
bool isNumberedListBegin(const char *s, const ll len, const L lbegin) {
    L l = lbegin;
    while(l.si < len && isdigit(s[l.si])) { l = l.next(s[l.si]); }
    // we made progress, didn't hit EOF, and have a "."
    return l.si > lbegin.si && l.si < len && s[l.si] == '.';
}



// TODO: convert \vert into |
// TODO: preprocess and check that we don't have \t tokens anywhere.
T* tokenizeBlock(const char *s, const ll len, const L lbegin) {
    assert(lbegin.si < len);
    L lcur = lbegin;

    if (lcur.si < len - 1 && s[lcur.si] == '\n' && s[lcur.si+1] == '\n') {
        while(lcur.si < len - 1 && (s[lcur.si+1] == '\n' || s[lcur.si+1] == ' ' || s[lcur.si+1] == '\t')) {
            lcur = lcur.next(s[lcur.si]);
        }
        return new T(TT::LineBreak, Span(lbegin, lcur));
    }
    if (s[lcur.si] == '\n') {
        return new T(TT::RawText, Span(lcur, lcur.nextline()));
    }
    if (strpeek(s + lcur.si, "$$")) {
        lcur = lcur.next("$$");

        // TODO: fix error message here, that will get generated from strconsume.
        // I had never thought about the problem that occurs when the opening
        // and closing braces are the same...
        lcur = strconsume(lcur, s, "$$", "unclosed $$ tag.");

        // we need to have $$\n
        if (lcur.si < len && s[lcur.si] != '\n') {
            printferr(lcur, s, "incorrectly terminated $$."
                        "must have newline following.");
            assert(false && "incorrectly terminated $$");
        }
        return new T(TT::LatexBlock, Span(lbegin, lcur));
    }
    else if (strpeek(s + lcur.si, "<script")) {
        lcur = strconsume(lcur, s, "</script>", "unclosed <script> tag.");
        return new T(TT::HTML, Span(lbegin, lcur));
    }
    else if (strpeek(s + lcur.si, "<!--")) {
        lcur = strconsume(lcur, s, "-->", "unclosed comment till end of file.");
        return new T(TT::Comment, Span(lbegin, lcur));
    }
    else if (strpeek(s + lcur.si, "```")) {
        lcur = lcur.next("```");

        const int LANG_NAME_SIZE = 20;
        char langname[LANG_NAME_SIZE];
        ll langlen = 0;
        while(s[lcur.si] != '\n' && langlen < LANG_NAME_SIZE-1) {
            langname[langlen++] = s[lcur.si];
            lcur = lcur.next(s[lcur.si]);
        }
        assert(langlen <= LANG_NAME_SIZE-1);
        langname[langlen] = 0;

        // error out if the language name is too long.
        if(langlen == LANG_NAME_SIZE-1) {
            printferr(lbegin, s, "``` has too long a language name: |%s|", langname);
            assert(false && "too long a language name");
        }

        // default language is text.
        if (langlen == 0) { strcpy(langname, ""); }


        assert(s[lcur.si] == '\n');
        lcur = strconsume(lcur, s, "```", "unclosed code block tag.");

        // we need to have ```\n
        if (lcur.si < len && s[lcur.si] != '\n') {
            printferr(lcur, s, "incorrectly terminated ```."
                        "must have newline following.");
            assert(false && "incorrectly terminated code block.");
        }

        return new TCodeBlock(Span(lbegin, lcur), langname);
    } 
    else if (strpeek(s + lcur.si, "#")) {
        cerr << "HEADING" << lcur << "\n";
        int i = 0;
        for(;lcur.si < len && s[lcur.si] == '#'; lcur = lcur.next('#')){ i++;};
         T *t = tokenizeInlineTill(s, len, lcur, [](const char *s, L lcur) {
                 cerr << "HEDING-CHECK" << lcur << " " << s[lcur.si] << "\n";
                 return s[lcur.si] == '\n';
         });
        cerr << "HEADING" << lcur << "\n";
         return new THeading(i, Span(lcur, t->span.end), t);
    } else if (s[lcur.si] == '-' && (lcur.si == 0 || s[lcur.si - 1] == '\n')) {
        vector<T*> toks;
        toks.push_back(tokenizeHyphenListItem(s, len, lcur));
        lcur = toks[0]->span.end;

        // TODO: allow lists of the form
        // - item
        // <newline>
        // <newline>
        // - item
        // to be counted as *the same list*.
        // as long as we have items..
        while(s[lcur.si] == '\n' && s[lcur.si+1] == '-') {
            lcur = lcur.next("\n");
            toks.push_back(tokenizeHyphenListItem(s, len, lcur));
            lcur = (*toks.rbegin())->span.end;
        }

        return new TList(toks);
    } 
    else if ((lcur.si == 0 || s[lcur.si - 1] == '\n') && 
            isNumberedListBegin(s, len, lcur)) {
        vector<T*> toks;
        int curnum = 1;
        toks.push_back(tokenizeNumberedListItem(s, len, lcur, curnum++));
        lcur = toks[0]->span.end;

        // as long as we have items..
        while(s[lcur.si] == '\n' && 
                isNumberedListBegin(s, len, lcur.nextline())) {
            lcur = lcur.next("\n");
            toks.push_back(tokenizeNumberedListItem(s, len, lcur, curnum++));
            lcur = (*toks.rbegin())->span.end;
        }

        return new TListNumbered(toks);
    } 
    else if (s[lcur.si] == '>' && (lcur.si == 0 || s[lcur.si - 1] == '\n')) {
        // quotes
        vector<T*>toks;
        toks.push_back(tokenizeQuoteItem(s, len, lcur));
        lcur = toks[0]->span.end;

        // as long as we have items..
        while(s[lcur.si] == '\n' && s[lcur.si+1] == '>') {
            lcur = lcur.next("\n");
            toks.push_back(tokenizeQuoteItem(s, len, lcur));
            lcur = (*toks.rbegin())->span.end;
        }

        return new TQuote(Span(lbegin, lcur), toks);
    }
    else {
        return tokenizeNext(s, len, lbegin);
    }
}


void tokenize(const char *s, const ll len, vector<T*> &ts) {
    Span span(lfirstline, lfirstline);
    while (span.end.si < len) {
        T *t = tokenizeBlock(s, len, span.end);
        assert(t != nullptr);
        ts.push_back(t);
        cerr << *t << "\n";

        // we always have to make progress.
        assert(span.end.si != t->span.end.si);
        span = t->span;
    }
}


char* pygmentize(const char *tempdirpath, 
        const char *code, int codelen, const char *lang, const char *instr, const L loc) {


    char latex_file_path[512];
    sprintf(latex_file_path, "%s/input.txt", tempdirpath);
    FILE *f = fopen(latex_file_path, "w");
    assert(f && "unable to open temp file");
    ll nwritten = fwrite(code, 1, codelen, f);
    assert(nwritten == codelen);
    fclose(f);

    // cerr << "wrote pygmentize input to: |" << latex_file_path << "|\n";

    char output_file_name[512];
    sprintf(output_file_name, "%s/input.txt.html", tempdirpath);

    int pid;
    // source-highlight -s cpp  /tmp/foo.py && cat /tmp/foo.py.html
    if ((pid = fork()) == 0) {
        int err = execlp("source-highlight",
                "source-highlight",
                "--style-css=./mono.css", // style provided as css file.
                "-n", // line numbers
                "-s", lang,  // source lang
                latex_file_path,
                NULL);
        assert(err != -1 && "unable to write pygments file.");
    } else {
        // parent, wait for child.
        int status;
        wait(&status);
        if(WIFEXITED(status) && (WEXITSTATUS(status) != 0)) {
            printferr(loc, instr, "unable to pygmentize code");
            assert(false && "child ended non-gracefully");
        };
    }

    f = fopen(output_file_name, "r");
    // cleanup.
    if (f == nullptr) { rmdir(tempdirpath); }
    assert(f && "unable to open output file of pygmentize");

    fseek(f, 0, SEEK_END); const ll len = ftell(f); fseek(f, 0, SEEK_SET);
    char *outbuf = (char *)calloc(len+1, sizeof(char));
    assert(outbuf != nullptr && "unable to allocate buffer");

    const ll nread = fread(outbuf, 1, len, f);
    assert(nread == len);
    fclose(f);
    return outbuf;
};

// TODO: fix representation to hold the full span, and the span
// of the inner data separately.
// convert:
// $$\begin{align*} .. \end{align*}$$
//   TO
// \begin{align*} .. \end{align*}
pair<ll, L> removeAlignDollarsHack(const char *instr, const ll inwritelen,
        const L loc) {
    assert(instr[loc.si] == '$');

    // we may have inline latex that does not have $$.
    if(!strpeek(instr + loc.si, "$$")) { return make_pair(inwritelen, loc); }
    int ix = loc.si + 2;
    while(instr[ix] == ' ' || instr[ix] == '\n' || instr[ix] == '\t') { ++ix; }
    const char *BEGINALIGN = "\\begin{align*}";
    if (strpeek(instr + ix, BEGINALIGN)) {
        fprintf(stderr, "|||");
        for(int i = 0; i < inwritelen-4; ++i) {
            fprintf(stderr, "%c", instr[loc.next("$$").si + i]);
        }
        fprintf(stderr, "|||");
        return make_pair(inwritelen - 4, loc.next("$$"));
    }
    return make_pair(inwritelen, loc);
};


GIVE char* compileLatex(KEEP const char *tempdirpath, ll inwritelen, 
        KEEP const char *instr, L loc) {

    tie(inwritelen, loc) = removeAlignDollarsHack(instr, inwritelen, loc);

    const ll hash = hashstr(instr + loc.si, inwritelen);
    if (const char *val = lookup_key(hash)) {
        return strdup(val);
    }


    FILE *flatex = nullptr, *fhtml = nullptr;
    char latex_file_path[512]; char html_file_path[512];
    sprintf(latex_file_path, "%s/latex-in.txt", tempdirpath);
    sprintf(html_file_path, "%s/latex-out.txt", tempdirpath);

    flatex = fopen(latex_file_path, "wb");
    assert(flatex && "unable to open file for writing");
    fprintf(flatex, "\\usepackage{amsmath}\n");
    
    // SUPER HACK: Process the string to remove the $$
    // if we have a \begin{align*} right after.

    // fprintf(flatex, "\\usepackage{amssymb}\n");
    const ll nwritten = fwrite(instr + loc.si, 1, inwritelen, flatex);
    assert(nwritten == (ll)inwritelen);

    fclose(flatex);
    // cerr << "wrote latex input |";
    // for(int i = 0; i < inwritelen; ++i) { cerr << ins[i]; }
    // cerr << "| to: |" << latex_file_path << "|\n";

    int pid;
    // hevea STDIN
    if ((pid = fork()) == 0) {

        flatex = fopen(latex_file_path, "rb");
        assert(flatex && "unable to open input file for reading");

        fhtml = fopen(html_file_path, "wb");
        assert(fhtml && "unable to open input file for reading");

        // replace stdin with latex.
        dup2(fileno(flatex), 0);
        // replace stdout with outfile
        dup2(fileno(fhtml), 1);

        int err = execlp("hevea", "hevea", "-w", "100", NULL);
        assert(err != -1 && "unable to launch 'hevea'");
    } else {
        // parent, wait for child.
        // cerr << "waiting for child to terminate...";
        int status;
        wait(&status);
        if(WIFEXITED(status) && (WEXITSTATUS(status) != 0)) {
            fprintf(stderr, "===LATEX ERROR===\n");
            printferr(loc, instr, "unable to hevea (latexif render) code");
            cerr <<  "====child running hevea ended non-gracefully. Returning input string===\n";
            assert(false && "child ended non-gracefully.");
            char *outs = (char *)malloc((inwritelen+2) *sizeof(char));
            int i  = 0;
            for(; i < inwritelen; ++i) { outs[i] = instr[loc.si + i]; }
            outs[i] = 0; return outs;
        } 
        // else {
        //     cerr << "\rchild exited gracefully.\n";
        // }
    }

    fhtml = fopen(html_file_path, "rb");
    if (fhtml == nullptr) { rmdir(tempdirpath); }
    assert(fhtml && "unable to open output file of pygmentize");


    fseek(fhtml, 0, SEEK_END);
    const ll len = ftell(fhtml); fseek(fhtml, 0, SEEK_SET);
    char *outs = (char *)calloc(len+1, sizeof(char));
    assert(outs != nullptr && "unable to allocate buffer");
    const ll nread = fread(outs, 1, len, fhtml);
    assert(nread == len);
    // fprintf(stderr, "HTML output: %s\n", outs);
    fclose(fhtml);

    store_key_value(hash, outs, len);
    return outs;
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
void headingToLinkText(const char *instr, 
        const T *t, 
        char *outs,
        ll &outlen) {

    if (t->ty == TT::InlineGroup) {
        for(T *item : ((TInlineGroup *)t)->items) {
            headingToLinkText(instr, item, outs, outlen);
        }
    } else if (t->ty == TT::CodeInline) {
        Span span = Span(t->span.begin.next("`"),
                        t->span.end.prev("`"));
        strncpy(outs, instr + span.begin.si, span.nchars());
        outlen += span.nchars();
    } else if (t->ty == TT::LatexInline) {
        Span span = Span(t->span.begin.next("$"),
                        t->span.end.prev("$"));
        strncpy(outs, instr + span.begin.si, span.nchars());
        outlen += span.nchars();
    } else if (t->ty == TT::RawText) {
        strncpy(outs, instr + t->span.begin.si, t->span.nchars());
        outlen += t->span.nchars();
    } else if (t->ty == TT::Comment) {
        return;
    } else if (t->ty == TT::Bold) {
        TBold *bold = (TBold *)t;
        headingToLinkText(instr, bold->item, outs, outlen);
    } else if (t->ty == TT::Italic) {
        TItalic *italic = (TItalic *)t;
        headingToLinkText(instr, italic->item, outs, outlen);
    } else if (t->ty == TT::Link) {
        TLink *link = (TLink *)t;
        headingToLinkText(instr, link->text, outs, outlen);
    } else {
        cerr << "unexpected token in heading at: " << t->span;
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
// remove anything that is not a letter, number, space or hyphen (see the source for how Unicode is handled)
// changes any space to a hyphen.
// If that is not unique, add "-1", "-2", "-3",... to make it unique
GIVE const char *mkHeadingLink(KEEP const char *instr, KEEP THeading *heading) {
    const int BUFSIZE = 2048;
    char rawtext[BUFSIZE]; 
    for(int i = 0; i < BUFSIZE; ++i) rawtext[i] = 0;
    ll rawtextlen = 0;
    headingToLinkText(instr, heading->item, rawtext, rawtextlen);
    rawtext[rawtextlen] = 0;
    assert(rawtextlen + 1 < BUFSIZE && "heading exceeded buffer size limits");

    char *link = (char *)calloc(rawtextlen +2, sizeof(char));
    ll li = 0;
    bool seenalnum = false;
    for(ll hi = 0; rawtext[hi] != '\0'; ++hi) {
        // convert uppercase -> lowercase
        // keep digits
        // convert space to hyphen
        // convert groups of hyphens into single hyphen
        // remove everything else.
        const char c = rawtext[hi];
        if (isalpha(c)) { link[li++] = tolower(c); seenalnum = true; }
        else if (isdigit(c)) { link[li++] = c; seenalnum = true; }
        else if (c == '-') { 
            while (rawtext[hi+1] == '-') { hi++; } link[li++] = '-'; seenalnum = false; 
        } else if (isspace(c) && seenalnum) { link[li++] = '-'; seenalnum = false; }
    }
    return link;
}

void toHTML(const char *instr,
        const char *tempdirpath, 
        const T *t, ll &outlen, char *outs) {
    assert(t != nullptr);
    switch(t->ty) {
        case TT::Undefined: { assert(false && "Should not have received undefined"); return ; }
        case TT::Comment: return;

        case TT::HTML:
        case TT::RawText:
        strncpy(outs + outlen, instr + t->span.begin.si, t->span.nchars());
        outlen += t->span.nchars();
        return;

        case TT::LineBreak: {
        // HACK!
        return;
          const char *br = "<br/>"; 
          strcpy(outs + outlen, br);
          outlen += strlen(br);
          return;
        }

        case TT::CodeBlock: {
          TCodeBlock *block = (TCodeBlock *)t;
          // TODO: escape HTML content.
          const char *open = "<div class='code'>";
          const char *close = "</div>";

          strcpy(outs + outlen, open);
          outlen += strlen(open);

          // we want to ignore the first 3 ``` and the last 3 ```
          const Span span =
              Span(t->span.begin.next("```").next(block->langname),
                      t->span.end.prev("```"));
          char *code_html = pygmentize(tempdirpath,
                  instr + span.begin.si,
                  span.nchars(), block->langname,
                  instr, t->span.begin);

          strcpy(outs + outlen, code_html);
          outlen += strlen(code_html);
          free(code_html);

          strcpy(outs + outlen, close);
          outlen += strlen(close);
          return;
        }

        case TT::LatexInline: 
        case TT::LatexBlock: {
          // const Span span = Span(t->span.begin.next("$"), t->span.end.prev("$"));
          const Span span = t->span; 
          if (t->ty == TT::LatexBlock) { 
              outlen += sprintf(outs + outlen, "<div class='latexblock'>");
          } else if (t->ty == TT::LatexInline) {
              outlen += sprintf(outs + outlen, "<span class='latexinline'>");
          }

          if (G_OPTIONS.latex2ascii || true) {
              char *outcompile = compileLatex(tempdirpath,
                      span.nchars(),
                      instr, t->span.begin);
              strcpy(outs + outlen, outcompile);
              outlen += strlen(outcompile);
              free(outcompile);
          } else {
              strncpy(outs + outlen, instr + t->span.begin.si, t->span.nchars());
              outlen += t->span.nchars();
          }

          if (t->ty == TT::LatexBlock) { 
              outlen += sprintf(outs + outlen, "</div>");
          } else if (t->ty == TT::LatexInline) { 
              outlen += sprintf(outs + outlen, "</span>");
          }
          return;
        }

        // case TT::LatexBlock: {
        //   strncpy(outs + outlen, ins + t->span.begin.si, t->span.nchars());
        //   outlen += t->span.nchars();
        //   return;
        // }

        case TT::List: {
          const char *openul = "<ul>";
          const char *closeul = "</ul>";

          const char *openli = "<li>";
          const char *closeli = "</li>";

          TList *tlist = (TList *)t;
          strcpy(outs + outlen, openul); outlen += strlen(openul);
          for(auto it: tlist->items) {
              strcpy(outs + outlen, openli); outlen += strlen(openli);
              toHTML(instr, tempdirpath, it, outlen, outs);
              strcpy(outs + outlen, closeli); outlen += strlen(closeli);
          }
          strcpy(outs + outlen, closeul); outlen += strlen(closeul);
          return;
        }

        case TT::TListNumbered: {
          const char *openul = "<ol>";
          const char *closeul = "</ol>";

          const char *openli = "<li>";
          const char *closeli = "</li>";

          TList *tlist = (TList *)t;
          strcpy(outs + outlen, openul); outlen += strlen(openul);
          for(auto it: tlist->items) {
              strcpy(outs + outlen, openli); outlen += strlen(openli);
              toHTML(instr, tempdirpath, it, outlen, outs);
              strcpy(outs + outlen, closeli); outlen += strlen(closeli);
          }
          strcpy(outs + outlen, closeul); outlen += strlen(closeul);
          return;
        }

        case TT::Link: {
          TLink *link = (TLink *)t;
          // toHTML(instr, tempdirpath, link->text,  outlen, outs);
          outlen += sprintf(outs + outlen, "<a href=%s>", link->link);
          toHTML(instr, tempdirpath, link->text, outlen, outs);
          outlen += sprintf(outs + outlen, "</a>");
          return;
        }

        case TT::InlineGroup: {
            TInlineGroup *group = (TInlineGroup *)t;
            for (T *t : group->items) { 
                toHTML(instr, tempdirpath, t, outlen, outs);
            }
            return;
        }


        case TT::CodeInline: {
            const char *open =  "<code>";
            const char *close = "</code>";
            strcpy(outs + outlen, open); outlen += strlen(open);

            const Span span =
                Span(t->span.begin.next("`"), t->span.end.prev("`"));

            strncpy(outs + outlen, instr + span.begin.si, span.nchars());
            outlen += span.nchars();

            strcpy(outs + outlen, close); outlen += strlen(close);
            return;
        }

        case TT::Heading: {
            THeading *theading = (THeading *)t;

            // need the _raw text_. Hmm.
            const char *link = mkHeadingLink(instr, theading);
            // outlen += sprintf(outs + outlen, "<h%d id=%s>", theading->hnum, link);
            outlen += sprintf(outs + outlen, "<h%d>", min(4, 1+theading->hnum));
            outlen += sprintf(outs + outlen, "<a id=%s href='#%s'> %s </a>", link, link, "§");
            toHTML(instr, tempdirpath, theading->item, outlen, outs);
            outlen += sprintf(outs + outlen, "</h%d>", min(4, 1+theading->hnum));

            free((char *)link);
            return;
        }

        case TT::Italic: {
            TItalic *tcur = (TItalic *)t;
            outlen += sprintf(outs + outlen, "<i>");
            toHTML(instr, tempdirpath, tcur->item, outlen, outs);
            outlen += sprintf(outs + outlen, "</i>");
            return;
        }

        case TT::Bold: {
            TBold *tcur = (TBold *)t;
            outlen += sprintf(outs + outlen, "<b>");
            toHTML(instr, tempdirpath, tcur->item, outlen, outs);
            outlen += sprintf(outs + outlen, "</b>");
            return;
        }

        case TT::Quote: {
          TQuote *tq = (TQuote *)t;

          outlen += sprintf(outs + outlen, "<blockquote>");
          for(auto it: tq->items) {
              toHTML(instr, tempdirpath, it, outlen, outs);
          }
          outlen += sprintf(outs + outlen, "</blockquote>");
          return;
        }

        // default: {
        //     cerr << "outbuf:\n=========\n";
        //     cerr << outs;
        //     cerr << "\n";
        //     cerr << "========\n";
        //     cerr << "token: " << *t << " | is unknown for toHTML.\n"; 
        //     assert(false && "unknown token");
        // }
    };
    assert(false && "unreachabe");
}

// TUFTE
// <body vlink="#660000" text="#000000" link="#CC0000"
//  bgcolor="#FFFFF3" alink="#660000">
const char htmlbegin[] =
"<!DOCTYPE html>"
"<meta charset='UTF-8'>"
"<html>"
"<head>"
"<title> A Universe of Sorts </title>"
"<style>"
"@font-face {font-family: 'Blog Mono'; src: url('/static/iosevka-fixed-extendedmedium.ttf');}"
"@font-face {font-family: 'Blog Text'; src: url('/static/iosevka-etoile-regular.ttf');}"
// body
"html { font-size: 100%; }"
"html,body { text-size-adjust: none; -webkit-text-size-adjust: none; -moz-text-size-adjust: none; -ms-text-size-adjust: none; } "
"body {"
" background-color: #FFFFFF; color: #000000; " // tufte
" font-family: 'Blog Text', monospace; font-size: 20px; line-height: 1.4em; "
" max-width: 100%; }"
"\n"
// img as a block
"img { display:block; }"
// container
".container { margin-left: 20%; margin-right: 10%; }"
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
"pre, .latexblock, blockquote { border-left-color:#000000;  border-left-style: solid;"
"      border-left-width: 4px; padding-left: 5px; }"
"\n"
// need margin:0px otherwise user agent stylesheet will fuck it up
" blockquote { color: #222222; margin: 0px; }"
// monospace font
".latexblock, .latexinline, .code { font-family: 'Blog Mono', monospace; line-height: 1.2em; font-size: 80%;  }"
// padding and margin for blocks
".latexblock, blockquote, .code, code { margin-top: 30px; margin-bottom: 30px; padding-bottom: 5px; padding-top: 5px; background-color: #FFFFFF; }"
".code, code { background-color: #FFFFFF; width: 100%; }"
// overflow: latex and code block
" .latexblock {  width: 100%; overflow-x: auto; white-space: nowrap; }"
// " .code { width: 100%; overflow-x: hidden; white-space: nowrap; }"
" .code pre { width: 100%; overflow-x: auto; margin: 0px; overflow-y: hidden; padding-top: 5px; padding-bottom: 5px; }"
"\n"
".latexinline { border-bottom-color: #000000; border-bottom-style: solid;"
"                border-bottom-width: 1px; padding-bottom: 2px;"
"                padding-left: 2px; padding-right: 2px; }"
// fix font handling
"pre, code, kbd, samp, tt{ font-family:'Blog Mono',monospace; }"
// RESPONSIVE
// " @media (max-width: 1000px) {"
// "    .container { max-width: 100%; padding: 0; margin-left: 10%; margin-right: 0px;"
// "                 padding-left: 0px; padding-right: 0px; border: none; }"
// "}"
// HEVEA
".latexblock .li-itemize{margin:1ex 0ex;}"
".latexblock .li-enumerate{margin:1ex 0ex;}"
".latexblock .footnotetext{margin:0ex; padding:0ex;}"
".latexblock div.footnotetext P{margin:0px; text-indent:1em;}"
".latexblock .thefootnotes{text-align:left;margin:0ex;}"
".latexblock .dt-thefootnotes{margin:0em;}"
".latexblock .dd-thefootnotes{margin:0em 0em 0em 2em;}"
".latexblock .footnoterule{margin:1em auto 1em 0px;width:50%;}"
".latexblock .caption{padding-left:2ex; padding-right:2ex; margin-left:auto; margin-right:auto}"
".latexblock .title{margin:2ex auto;text-align:center}"
".latexblock .titlemain{margin:1ex 2ex 2ex 1ex;}"
".latexblock .center{text-align:center;margin-left:auto;margin-right:auto;}"
".latexblock .flushleft{text-align:left;margin-left:0ex;margin-right:auto;}"
".latexblock .flushright{text-align:right;margin-left:auto;margin-right:0ex;}"
".latexblock div table{margin-left:inherit;margin-right:inherit;margin-bottom:2px;margin-top:2px}"
".latexblock td table{margin:auto;}"
".latexblock table{border-collapse:collapse;}"
".latexblock td{padding:0;}"
".latexblock .cellpadding0 tr td{padding:0;}"
".latexblock .cellpadding1 tr td{padding:1px;}"
".latexblock pre{text-align:left;margin-left:0ex;margin-right:auto;}"
".latexblock blockquote{margin-left:4ex;margin-right:4ex;text-align:left;}"
".latexblock td p{margin:0px;}"
".latexblock .hbar{border:none;height:2px;width:100%;background-color:black;}"
".latexblock .display{border-collapse:separate;border-spacing:2px;width:auto; border:none;}"
".latexblock .dcell{white-space:nowrap;padding:0px; border:none;}"
".latexblock .dcenter{margin:0ex auto;}"
".latexblock .theorem{text-align:left;margin:1ex auto 1ex 0ex;}"
".latexblock .tst{font-family:sans;font-style:oblique;color:maroon}"
".latexblock .highlight{color:lime}"
// end style
"</style>"
"</head>"
"<body>"
"<div class='container'>";
 

const char htmlend[] =
 "</container>"
 "</body>"
 "</html>";


int option_index(const int argc, char **argv, const char *opt) {
    for(int i = 1; i < argc; ++i) {
        if (!strcmp(argv[i], opt)) { return i; }
    }
    return 0;
}


T ts[MAX_TOKENS];
char instr[MAX_CHARS];
int main(int argc, char **argv) {
    // 1. Load options
    // ---------------
    if (argc < 3) {
        printf("expected usage: %s <input md path> <output folder path> [--latex2ascii]", argv[0]);
        return 1;
    }
    G_OPTIONS.latex2ascii = option_index(argc, argv, "--latex2ascii") > 0;

    // 1. Load database and test it.
    // ---------------
    loadDB();

    // 2. Open markdown file
    // ---------------------
    FILE *fin = fopen(argv[1], "rb");
    if (fin == nullptr) {
        printf("unable to open file: |%s|\n", argv[1]);
        return -1;
    }

    fseek(fin, 0, SEEK_END); const ll len = ftell(fin); fseek(fin, 0, SEEK_SET);
    assert(len < MAX_CHARS);
    cout << "Input length: |" << len << "|\n";

    const ll nread = fread(instr, 1, len, fin); assert(nread == len);

    vector<T*> ts; tokenize(instr, nread, ts);
    cerr << "Done tokenizing; now parsing...\n";


    char tempdirpath[100] = "/tmp/blog-build-XXXXXX";
    const char *success = mkdtemp(tempdirpath);
    assert(success != nullptr && "unable to create temporary directory");

    ll MAX_OUTBUF_LEN = (ll)1e8L;
    char *outbuf = (char *)calloc(MAX_OUTBUF_LEN, sizeof(char));
    ll outlen = 0;
    outlen += sprintf(outbuf + outlen, "%s", htmlbegin);
    for(T * t : ts) { toHTML(instr, tempdirpath, t, outlen, outbuf); }
    outlen += sprintf(outbuf + outlen, "%s", htmlend);

    rmdir(tempdirpath);


    FILE *fout = fopen(argv[2], "wb");
    if (fout == nullptr) {
        fprintf(stderr, "unable to open output file: |%s|", argv[2]);
    }
    assert(fout != nullptr);
    fwrite(outbuf, 1, strlen(outbuf), fout);
    fclose(fout);
    
    return 0;
}
