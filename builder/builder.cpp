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
#include <vector>
#include <utility>
#include <tuple>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
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
enum class ET { Link, List, Text, Heading, Block };


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
    Link,
    ListItem,
    List
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
        case TT::Link: return o << "LINK";
        case TT::ListItem: return o << "LISTITEM";

        default: assert((int) ty <= 128); return o << "TY(" << (char)ty << ")";
    }
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

    Span extend(const Span next) {
        assert(next.begin.si >= end.si);
        return Span(begin, next.end);
    }
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

    cerr << "> ";
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

struct TList : public T {
    vector<T*> items;
    TList(vector<T*> items) : T(TT::List, makeListSpan(items)) { };

    static Span makeListSpan(vector<T*>items) {
        assert(items.size() > 0);
        Span s = items[0]->span;
        for(int i = 1; i < items.size(); ++i) { s = s.extend(items[i]->span); }
        return s;
    }
};

struct TCode : public T {
    TCode(Span span, const char *langname) : 
        T(TT::CodeBlock, span), langname(strdup(langname)) {}
    char *langname;
};

struct E {
    public: ET type; virtual void print(std::ostream &o, int depth) = 0;
    protected: E (ET type) : type(type) {};
};

void printspaces(ostream &o, int n) { for(int i=0;i<n;++i) o<<" "; }
struct EList : public E {
    std::vector<E*> es;
    EList(std::vector<E*> es) : E(ET::List), es(es)  {}
    virtual void print(std::ostream &o, int depth=0) {
        o << "EList(\n";
        for(E *e: es) { e->print(o, depth+1); o << "\n"; }
        o << "\n"; printspaces(o, depth); o << ")";
    }
};

struct ELink : public E {
    E *text, *link;
};
struct EText : public E {
    T text; // raw text or equivalent
    EText(T text) : E(ET::Text), text(text) {}
    virtual void print(std::ostream &o, int depth)  {
        o << text;
    }
};

struct EHeading : public E {
    E*inner;
};

struct EBlock : public E {
    vector<E*> es;
    EBlock(vector<E*> es) : E(ET::List) {}; 
};



bool is_char_special_token(char c) {
        return  c == '*' || c == '[' || c == '<' || c == '>' || c == '$' ||
        c == '`' || c == '\n';
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
        assert(strpeek(filestr + l.si, delim));
        l = l.next(delim);
    }
    return l;
}

pair<bool, T*> tokenize(const char *s, const ll len, const L lbegin, const bool prevnewline);

// tokenize those strings that can only occur "inside" an inline context,
// so only:
// - raw text
// - inline math
// - bold/italic/underline
T *tokenizeLink(const char *s, const ll len, const L opensq) {
    assert(opensq.si < len);
    assert(s[opensq.si] == '[');
    
    L closesq = opensq;
    while(s[closesq.si] != ']' && s[closesq.si] != '\0')  { 
        closesq = closesq.next(s[closesq.si]);
    }
    
    if (s[closesq.si] != ']') { return nullptr; }
    if (!(closesq.si + 1 < len && s[closesq.si + 1] == '(')) { return nullptr; }
    const L openround = closesq.next(s[closesq.si]);
    if(s[openround.si] != '(') { return nullptr; };

    L closeround = openround;
    while(s[closeround.si] != ')' && s[closeround.si] != '\0')  { 
        closeround = closeround.next(s[closeround.si]);
    }
    if (s[closeround.si] != ')') { return nullptr; }

    char *link = (char *)malloc(sizeof(char) * (closeround.si - openround.si));
    for(int i = 0, j = openround.si+1; s[j] != ')'; ++i, ++j) {
        link[i] = s[j];
    }
    
    T* text;
    // TODO: change `tokenize` to `tokenizeInline` when the time is right.
    // std::tie(newline, text) = tokenize(s, closesq.si - opensq.si, opensq, false);
    text = new T(TT::RawText, Span(opensq, closesq));
    return new TLink(Span(opensq, closeround), text, link);
}

// we are assuming that this is called on the *first* list item that
// has been seen.
T* tokenizeListItem (const char *s, const ll len, const L lhyphen) {
    
    assert(s[lhyphen.si] == '-');

    const L ltextbegin = lhyphen.nextcol();
    L lend = lhyphen.nextcol();

    bool foundNextItem = false; L lnexthypen = lundefined;
    while(lend.si < len) {
    
        // two spaces indicates the end of a list.
        if (lend.si < len - 1 &&
            s[lend.si] == '\n' &&
            s[lend.si+1] == '\n') { 
            cerr << __LINE__ << "|" << lend << "\n";
            break;
        }

        // newline and hyphen indicates another list item.
        // TODO: write validation to rule out things like:
        // <NEWLINE><SP>-
        if (lend.si < len - 1 && 
            s[lend.si] == '\n' && s[lend.si+1] == '-') {
            foundNextItem = true;
            lnexthypen = lend.next("\n");
            break;
        }

        // newline and space indicates continuation of same
        // list item text.
        if (lend.si < len - 1 && 
            s[lend.si] == '\n' &&
            s[lend.si+1] != ' ') {
            printferr(lend.nextline(), s,
                "unknown character in hanging list item: |%c| (are you missing alignment with the `-` ?)",
                s[lend.si+1]);
            assert(false && "unknown character in hanging list item.");
        }

        // consume;
        lend = lend.next(s[lend.si]);
    }
    
    assert(s[lend.si] == '\n');
    return new T(TT::RawText, Span(ltextbegin, lend));
}



// TODO: convert \vert into |
// TODO: preprocess and check that we don't have \t tokens anywhere.
pair<bool, T*> tokenize(const char *s, const ll len, const L lbegin, const bool prevnewline) {
    assert(lbegin.si < len);
    L lcur = lbegin;

    if (strpeek(s + lcur.si, "$$")) {
        lcur = lcur.next("$$");
        if (!prevnewline) {
            printferr(lcur, s, "$$ can only be opened on newline.");
            assert(false);
        }

        // TODO: fix error message here, that will get generated from strconsume.
        // I had never thought about the problem that occurs when the opening
        // and closing braces are the same...
        lcur = strconsume(lcur, s, "$$", "unclosed $$ tag.");
        return make_pair(false, new T(TT::LatexBlock, Span(lbegin, lcur)));
    }
    else if (strpeek(s + lcur.si, "<script")) {
        lcur = strconsume(lcur, s, "</script>", "unclosed <script> tag.");
        return make_pair(false, new T(TT::HTML, Span(lbegin, lcur)));
    }
    else if (strpeek(s + lcur.si, "<!--")) {
        lcur = strconsume(lcur, s, "-->", "unclosed comment till end of file.");
        return make_pair(false, new T(TT::Comment, Span(lbegin, lcur)));
    }
    else if (strpeek(s + lcur.si, "```")) {
        if (!prevnewline) {
            printferr(lcur, s, "``` can only be opened on newline.");
            assert(false);
        }

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
        }


        // default language is text.
        if (langlen == 0) { strcpy(langname, "text"); }


        assert(s[lcur.si] == '\n');
        lcur = strconsume(lcur, s, "```", "unclosed code block tag.");
        return make_pair(false, new TCode(Span(lbegin, lcur), langname));
    }
    else if (s[lcur.si] == '\n') { // this kills off newlines.
        return make_pair(true, new T(TT::Newline, Span(lbegin, lcur.nextline())));
    }
    else if (s[lcur.si] == '[') {
        return make_pair(false, new T(TT::OpenSquare, Span(lbegin, lcur.nextcol())));
    }
    else if (s[lcur.si] == ']') {
        return make_pair(false, new T(TT::CloseSquare, Span(lbegin, lcur.nextcol())));
    }
    else if (s[lcur.si] == '(') {
        return make_pair(false, new T(TT::OpenRound, Span(lbegin, lcur.nextcol())));
    }
    else if (s[lcur.si] == ')') {
        return make_pair(false, new T(TT::CloseSquare, Span(lbegin, lcur.nextcol())));
    }
    else if (prevnewline && s[lcur.si] == '-') {
        T *TListItem =  tokenizeListItem(s, len, lcur);
        return make_pair(false, TListItem);
        // return make_pair(false, new T(TT::ListHyphen, Span(lbegin, lcur.nextcol())));
    }
    else if (s[lcur.si] == '`') {
        lcur = lcur.nextcol();
        // TODO: fix error message here. 
        lcur = strconsume(lcur, s, "`", "unclosed inline code block `...`");

        if (lbegin.line != lcur.line) {
            printferr(lbegin, s, "inline code block `...` not allowed to be on two different lines."); 
            assert(false && "inline code block `...` on two different lines.");
        }

        return make_pair(false, new T(TT::CodeInline, Span(lbegin, lcur)));
    }
    else if (s[lcur.si] == '$') { // order is important; this should come here, after $$ has been tried.
        lcur = lcur.nextcol();
        // TODO: fix error message here. 
        lcur = strconsume(lcur, s, "$", "unclosed inline latex block $");

        if (lbegin.line != lcur.line) {
            printferr(lbegin, s, "inline latex block not allowed to be on two different lines."); 
            assert(false && "inline latex block on two different lines.");
        }

        return make_pair(false, new T(TT::LatexInline, Span(lbegin, lcur)));
    } else {
        // consume till a newline, or till a special char. If it _were_
        // part of a special form as the _first_ character, it would
        // have been consumed already. Hence, a do-while loop.
        do { 
            lcur = lcur.next(s[lcur.si]);
        } while(!is_char_special_token(s[lcur.si]) && s[lcur.si] != '\0');
        return make_pair(false, new T(TT::RawText, Span(lbegin, lcur)));
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

void tokenize(const char *s, const ll len, vector<T*> &ts) {
    Span span(lfirstline, lfirstline);
    bool prevnewline = true;
    while (span.end.si < len) {
        T *t = nullptr;
        // start tokenizing from the end of the prev span.
        std::tie(prevnewline, t) = tokenize(s, len, span.end, prevnewline);
        assert(t != nullptr);
        ts.push_back(t);
        cerr << *t << "\n";

        // we always have to make progress.
        assert(span.end.si != t->span.end.si);
        span = t->span;
    }
}

// BLOCK = LISTS BLOCK | RAW BLOCK
// LISTS = HYPHEN SPACE 

E *parse(const vector<T> &ts, ll &tix, const ll tend) {
    while(tix < tend) {
        const T tbegin = ts[tix];
        cerr << "@tix:" << tix << " line: " << __LINE__ << "\n";
        getchar();
                
        if (tbegin.ty == TT::HTML || 
            tbegin.ty == TT::RawText || 
            tbegin.ty == TT::LatexBlock || 
            tbegin.ty == TT::CodeBlock ||
            tbegin.ty == TT::Comment) {
            cerr << "@tix:" << tix << " line: " << __LINE__ << "\n";
            return new EText(ts[tix++]);
        } else if (ts[tix].ty == TT::ListHyphen) {
                cerr << "@tix:" << tix << " line: " << __LINE__ << "\n";
                // start parsing lists.
                vector<E*> es;
                tix += 1; // consume newline and hyphen

                do {
                    ll listend = tix;
                    // to end a hyphen, find 
                    // |- abc
                    // |  def (not ended: NEWLINE SPACE TOK)
                    // |g (ended: NEWLINE [NO SPACE] TOK)
                    while(listend < tend) {
                        if (listend < tend - 2 &&
                            ts[listend+1].ty == TT::Newline && 
                            ts[listend+2].ty != TT::Space) { break; } 
                        listend++;
                    }

                    // parse till the end of the list.
                    es.push_back(parse(ts, tix, listend));
                    tix = listend;
                } while(tix < tend && ts[tix+1].ty == TT::Newline && ts[tix+2].ty == TT::ListHyphen);

                assert(es.size() > 0 && "should have parsed at least one list.");
                return new EList(es);

        }
        // ignored tokens 
        else if (tbegin.ty == TT::Newline || tbegin.ty == TT::Space) { 
            cerr <<  "@" << __LINE__ << "\n";
            tix++;
            continue;
        }
        else {
            cerr << "token: " << ts[tix] << "\n";
            assert(false && "unknown token");
        }
    }
    return nullptr;
}

char* pygmentize(const char *code, int codelen, const char *lang) {

    char dirname[100] = "temp_XXXXXX";
    const char *temp_dirname = mkdtemp(dirname);
    assert(temp_dirname != nullptr &&
        "unable to create temporary directory");

    char input_file_path[512];
    sprintf(input_file_path, "%s/input.txt", dirname);
    FILE *f = fopen(input_file_path, "w");
    assert(f && "unable to open temp file");
    ll nwritten = fwrite(code, 1, codelen, f);
    assert(nwritten == codelen);
    fclose(f);

    cerr << "wrote pygmentize input to: |" << input_file_path << "|\n";



    char output_file_name[512];
    sprintf(output_file_name, "%s/input.txt.html", dirname);

    int pid;
    // source-highlight -s cpp  /tmp/foo.py && cat /tmp/foo.py.html
    if ((pid = fork()) == 0) {
        int err = execlp("source-highlight",
                "source-highlight",
                "-s", lang, 
                input_file_path,
                NULL);
        assert(err != -1 && "unable to write pygments file.");
    } else {
        // parent, wait for child.
        int status;
        wait(&status);
        if(WIFEXITED(status) && (WEXITSTATUS(status) != 0)) {
            assert(false && "child ended non-gracefully");
        };
    }

    f = fopen(output_file_name, "r");
    assert(f && "unable to open output file of pygmentize");


    char *outbuf = nullptr;
    fseek(f, 0, SEEK_END); const ll len = ftell(f); fseek(f, 0, SEEK_SET);
    outbuf = (char *)calloc(len+1, sizeof(char));
    assert(outbuf != nullptr && "unable to allocate buffer");

    const ll nread = fread(outbuf, 1, len, f);
    assert(nread == len);
    return outbuf;
};

void toHTML(const T *t, const char *filestr, ll &outlen, char *outs) {
    assert(t != nullptr);
    switch(t->ty) {
        case TT::HTML:
        case TT::RawText:
        case TT::Newline:
        case TT::Space:   
        case TT::Comment:
        strncpy(outs + outlen, filestr + t->span.begin.si, t->span.nchars());
        outlen += t->span.nchars();
        return;

        case TT::CodeBlock: {
          TCode *tcode = (TCode *)t;
          // TODO: escape HTML content.
          const char *code_block_open = "<code><pre>\n";
          const char *code_block_close = "</code></pre>\n";

          strcpy(outs + outlen, code_block_open);
          outlen += strlen(code_block_open);

          // we want to ignore the first 3 ``` and the last 3 ```
          cerr << __LINE__ << "\n";
          const Span span =
              Span(t->span.begin.next("```").next(tcode->langname),
                      t->span.end.prev("```"));
          char *code_html = pygmentize(filestr + span.begin.si,
                  span.nchars(), tcode->langname);

          strcpy(outs + outlen, code_html);
          outlen += strlen(code_html);

          strcpy(outs + outlen, code_block_close);
          outlen += strlen(code_block_close);
          return;
        }

        default:
            cerr << "outbuf:\n=========\n";
            cerr << outs;
            cerr << "\n";
            cerr << "========\n";
            cerr << "token: " << *t << " | is unknown for toHTML.\n"; 
            assert(false && "unknown token");
    };
    assert(false && "unreachabe");
}

T ts[MAX_TOKENS];
char filestr[MAX_CHARS];
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

    const ll nread = fread(filestr, 1, len, f);
    assert(nread == len);

    vector<T*> ts; tokenize(filestr, nread, ts);

    cerr << "done tokenizing; now parsing...\n";

    ll MAX_OUTBUF_LEN = (ll)1e8L;
    char *outbuf = (char *)calloc(MAX_OUTBUF_LEN, sizeof(char));
    ll outlen = 0;
    for(T * t : ts) {
        toHTML(t, filestr,  outlen, outbuf);
    }

    cerr << "output:\n=======\n";
    cerr << outbuf;
    cerr << "====\n";

    
    // ll tix = 0;
    // vector<E*> es;
    // while(tix < (ll)ts.size()) {
    //     E *e = parse(ts, tix, ts.size());
    //     if (!e) { break; }
    //      e->print(cerr, 0);
    //      es.push_back(e);
    // }
    
    return 0;
}
