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
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "utf8.h"
using ll = long long;
static const ll PADDING = 10;
static const ll MAX_TOKENS = 1e7;
static const ll MAX_CHARS = 1e7;


using namespace std;

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
    Group,
    InlineGroup,
    ListItemGroup,
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
        case TT::Group: return o << "GROUP";
        case TT::InlineGroup: return o << "INLINEGROUP";
        case TT::ListItemGroup: return o << "LISTTEMGROUP";
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

struct TCode : public T {
    TCode(Span span, const char *langname) : 
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
                return strpeek(s + lcur.si, delim);
            });

        lcur = item->span.end;
        if (lcur.si == len) {
            printferr(lbegin, s, "unmatched bold demiliter: |%s|.", delim);
            assert(false && "unmatched bold delimiter");
        }
        assert(lcur.si < len); 
        assert(strpeek(s + lcur.si, delim)); lcur = lcur.next(delim);

        if (lbegin.line != lcur.line) {
            printferr(lbegin, s, "bold delimiter |%s| spanning across lines.", delim);
            assert(false && "bold delimiter spans across lines");
        }

        return new TBold(Span(lbegin, lcur), item);
    }
    //This ordering is important; bold MUST be checked first.
    else if (s[lcur.si] == '*' || s[lcur.si] == '_') {
        const char c = s[lcur.si];
        T *item = tokenizeInlineTill(s, len, lcur.nextcol(), [c](const char *s, L lcur) {
                return s[lcur.si] == c;
        });

        lcur = item->span.end;
        if (lcur.si == len) {
            printferr(lbegin, s, "unmatched italic demiliter: |%c|.", c);
        }
        assert(lcur.si < len); assert(s[lcur.si] == c);
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
T* tokenizeListItem (const char *s, const ll len, const L lhyphen) {
    assert(s[lhyphen.si] == '-');
    const L ltextbegin = lhyphen.nextcol();
    return tokenizeInlineTill(s, len, ltextbegin, [lhyphen, len](const char *s, L lcur) {
            if (s[lcur.si] != '\n') { return false; }
            assert(s[lcur.si] == '\n');
            if (lcur.si + 1 >= len) { return true; }
            // file is still left. Check that what's going on next is legit..
            // TODO: consider moving this logic to be _ouside_ here.
            // should probably be in the caller of tokenizeListItem.
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

T* tokenizeQuoteItem (const char *s, const ll len, const L lquote) {
    assert(s[lquote.si] == '>');
    const L ltextbegin = lquote.nextcol();
    return tokenizeInlineTill(s, len, ltextbegin, [len, lquote](const char *s, L lcur) {
            if (s[lcur.si] != '\n') { return false; }
            assert(s[lcur.si] == '\n');
            if (lcur.si + 1 >= len) { return true; }
            // file is still left. Check that what's going on next is legit..
            // TODO: consider moving this logic to be _ouside_ here.
            // should probably be in the caller of tokenizeListItem.
            if (s[lcur.si + 1] == '>') { return true; }
            if (s[lcur.si + 1] == '\n') { return true; }
            printferr(lquote, s, 
                    "quote item must have:"
                    "\n- two newline separation"
                    "\n- quote continuation: '> ...'");
            assert(false && "list item ended improperly");
    });
}



// TODO: convert \vert into |
// TODO: preprocess and check that we don't have \t tokens anywhere.
T* tokenizeBlock(const char *s, const ll len, const L lbegin) {
    assert(lbegin.si < len);
    L lcur = lbegin;

    if (lcur.si < len - 1 && s[lcur.si] == '\n' && s[lcur.si+1] == '\n') {
        return new T(TT::LineBreak, Span(lcur, lcur.nextline().nextline()));
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
        if (langlen == 0) { strcpy(langname, "text"); }


        assert(s[lcur.si] == '\n');
        lcur = strconsume(lcur, s, "```", "unclosed code block tag.");

        // we need to have ```\n
        if (lcur.si < len && s[lcur.si] != '\n') {
            printferr(lcur, s, "incorrectly terminated ```."
                        "must have newline following.");
            assert(false && "incorrectly terminated code block.");
        }

        return new TCode(Span(lbegin, lcur), langname);
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
        toks.push_back(tokenizeListItem(s, len, lcur));
        lcur = toks[0]->span.end;

        // as long as we have items..
        while(s[lcur.si] == '\n' && s[lcur.si+1] == '-') {
            lcur = lcur.next("\n");
            toks.push_back(tokenizeListItem(s, len, lcur));
            lcur = (*toks.rbegin())->span.end;
        }

        return new TList(toks);
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
        const char *code, int codelen, const char *lang) {


    char latex_file_path[512];
    sprintf(latex_file_path, "%s/input.txt", tempdirpath);
    FILE *f = fopen(latex_file_path, "w");
    assert(f && "unable to open temp file");
    ll nwritten = fwrite(code, 1, codelen, f);
    assert(nwritten == codelen);
    fclose(f);

    cerr << "wrote pygmentize input to: |" << latex_file_path << "|\n";

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


char* compileLatex(const char *tempdirpath, const char *ins, 
    const ll inwritelen) {

    FILE *flatex = nullptr, *fhtml = nullptr;
    char latex_file_path[512]; char html_file_path[512];
    sprintf(latex_file_path, "%s/latex-in.txt", tempdirpath);
    sprintf(html_file_path, "%s/latex-out.txt", tempdirpath);

    flatex = fopen(latex_file_path, "wb");
    assert(flatex && "unable to open file for writing");
    const ll nwritten = fwrite(ins, 1, inwritelen, flatex);
    assert(nwritten == (ll)inwritelen);

    fclose(flatex);
    cerr << "wrote latex input |";
    for(int i = 0; i < inwritelen; ++i) { cerr << ins[i]; }
    cerr << "| to: |" << latex_file_path << "|\n";

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

        int err = execlp("hevea", "hevea", NULL);
        assert(err != -1 && "unable to launch 'hevea'");
    } else {
        // parent, wait for child.
        cerr << "waiting for child to terminate...";
        int status;
        wait(&status);
        if(WIFEXITED(status) && (WEXITSTATUS(status) != 0)) {
            cerr <<  "child running hevea ended non-gracefully. Returning input string...";
            char *outs = (char *)malloc((inwritelen+2) *sizeof(char));
            int i  = 0;
            for(;i < inwritelen; ++i) outs[i] = ins[i];
            outs[i] = 0; return outs;
        } else {
            cerr << "\rchild exited gracefully.\n";
        }
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

    fprintf(stderr, "HTML output: %s\n", outs);
    fclose(fhtml);
    return outs;
}

void toHTML(const char *tempdirpath, 
        const T *t, const char *ins, ll &outlen, char *outs) {
    assert(t != nullptr);
    switch(t->ty) {
        case TT::Comment: return;

        case TT::HTML:
        case TT::RawText:
        strncpy(outs + outlen, ins + t->span.begin.si, t->span.nchars());
        outlen += t->span.nchars();
        return;

        case TT::LineBreak: {
          const char *br = "<br/>"; 
          strcpy(outs + outlen, br);
          outlen += strlen(br);
          return;
        }

        case TT::CodeBlock: {
          TCode *tcode = (TCode *)t;
          // TODO: escape HTML content.
          const char *open = "<code>\n";
          const char *close = "</code>\n";

          strcpy(outs + outlen, open);
          outlen += strlen(open);

          // we want to ignore the first 3 ``` and the last 3 ```
          cerr << __LINE__ << "\n";
          const Span span =
              Span(t->span.begin.next("```").next(tcode->langname),
                      t->span.end.prev("```"));
          char *code_html = pygmentize(tempdirpath,
                  ins + span.begin.si,
                  span.nchars(), tcode->langname);

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
              outlen += sprintf(outs + outlen, "<div class='latex'>");
          }
          char *outcompile = compileLatex(tempdirpath,
                  ins + span.begin.si, span.nchars());
          strcpy(outs + outlen, outcompile);
          outlen += strlen(outcompile);
          if (t->ty == TT::LatexBlock) { 
              outlen += sprintf(outs + outlen, "</div>");
          }
          free(outcompile);
          return;
        }

        // case TT::LatexBlock: {
        //   strncpy(outs + outlen, ins + t->span.begin.si, t->span.nchars());
        //   outlen += t->span.nchars();
        //   return;
        // }

        case TT::List: {
          const char *openul = "<ul>\n";
          const char *closeul = "\n</ul>\n";

          const char *openli = "<li>\n";
          const char *closeli = "\n</li>\n";

          TList *tlist = (TList *)t;
          strcpy(outs + outlen, openul); outlen += strlen(openul);
          for(auto it: tlist->items) {
              strcpy(outs + outlen, openli); outlen += strlen(openli);
              toHTML(tempdirpath, it, ins, outlen, outs);
              strcpy(outs + outlen, closeli); outlen += strlen(closeli);
          }
          strcpy(outs + outlen, closeul); outlen += strlen(closeul);
          return;
        }

        case TT::Link: {
          TLink *link = (TLink *)t;
          outlen += sprintf(outs + outlen, "<a href=%s>\n", link->link);
          toHTML(tempdirpath, link->text, ins, outlen, outs);
          outlen += sprintf(outs + outlen, "</a>\n");
          return;
        }

        case TT::InlineGroup: {
            TInlineGroup *group = (TInlineGroup *)t;
            for (T *t : group->items) { 
                toHTML(tempdirpath, t, ins, outlen, outs);
            }
            return;
        }


        case TT::CodeInline: {
            const char *open =  "<code>";
            const char *close = "</code>";
            strcpy(outs + outlen, open); outlen += strlen(open);

            const Span span =
                Span(t->span.begin.next("`"), t->span.end.prev("`"));

            strncpy(outs + outlen, ins + span.begin.si, span.nchars());
            outlen += span.nchars();

            strcpy(outs + outlen, close); outlen += strlen(close);
            return;
        }

        case TT::Heading: {
            THeading *theading = (THeading *)t;
            outlen += sprintf(outs + outlen, "<h%d>", theading->hnum);
            toHTML(tempdirpath, theading->item, ins, outlen, outs);
            outlen += sprintf(outs + outlen, "</h%d>", theading->hnum);
            return;
        }

        case TT::Italic: {
            TItalic *tcur = (TItalic *)t;
            outlen += sprintf(outs + outlen, "<i>");
            toHTML(tempdirpath, tcur->item, ins, outlen, outs);
            outlen += sprintf(outs + outlen, "</i>");
            return;
        }

        case TT::Bold: {
            TBold *tcur = (TBold *)t;
            outlen += sprintf(outs + outlen, "<b>");
            toHTML(tempdirpath, tcur->item, ins, outlen, outs);
            outlen += sprintf(outs + outlen, "</b>");
            return;
        }

        case TT::Quote: {
          TQuote *tq = (TQuote *)t;

          outlen += sprintf(outs + outlen, "<blockquote>");
          for(auto it: tq->items) {
              toHTML(tempdirpath, it, ins, outlen, outs);
          }
          outlen += sprintf(outs + outlen, "</blockquote>");
          return;
        }

        default: {
            cerr << "outbuf:\n=========\n";
            cerr << outs;
            cerr << "\n";
            cerr << "========\n";
            cerr << "token: " << *t << " | is unknown for toHTML.\n"; 
            assert(false && "unknown token");
        }
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
 "body {"
 " background-color: #FFFFFA; color: #000000; " // tufte
 " font-family: monospace; font-size: 14px; line-height: 2em;"
 " width: 100ch; padding-left: 20%; padding-right: 20%;}"
 " @media screen and (max-width: 800px) { width: 100%; padding: 0"
 "}" // end body
 "a:hover { color: #CC0000; }" // hover
 "a { color: #AA0000; }" // unvisited; default
 "a:visited { color: #660000; }" // vlink
 "a:active { color: #660000; }" // alink
// code blocks, latex blocks
 "pre .latex { border-left-color:#660000;  border-left-style: solid;"
 "      border-left-width: 4px; padding-left: 5px; }" 
 // latex, we need line height to be correct
 ".latex { line-height: 1em; }"
 // end style
 "</style>"
 "</head>"
 "<body>";
 

const char htmlend[] =
 "</body>"
 "</html>";



T ts[MAX_TOKENS];
char filestr[MAX_CHARS];
int main(int argc, char **argv) {
    if (argc != 3) {
        printf("expected usage: %s <input md path> <output folder path>", argv[0]);
        return 1;
    }

    FILE *fin = fopen(argv[1], "rb");
    if (fin == nullptr) {
        printf("unable to open file: |%s|\n", argv[1]);
        return -1;
    }

    fseek(fin, 0, SEEK_END); const ll len = ftell(fin); fseek(fin, 0, SEEK_SET);
    assert(len < MAX_CHARS);
    cout << "Input length: |" << len << "|\n";

    const ll nread = fread(filestr, 1, len, fin); assert(nread == len);

    vector<T*> ts; tokenize(filestr, nread, ts);
    cerr << "Done tokenizing; now parsing...\n";


    char tempdirpath[100] = "temp_XXXXXX";
    const char *success = mkdtemp(tempdirpath);
    assert(success != nullptr && "unable to create temporary directory");

    ll MAX_OUTBUF_LEN = (ll)1e8L;
    char *outbuf = (char *)calloc(MAX_OUTBUF_LEN, sizeof(char));
    ll outlen = 0;
    outlen += sprintf(outbuf + outlen, "%s", htmlbegin);
    for(T * t : ts) { toHTML(tempdirpath, t, filestr,  outlen, outbuf); }
    outlen += sprintf(outbuf + outlen, "%s", htmlend);

    rmdir(tempdirpath);


    char outfilepath[512];
    if (mkdir(argv[2], 0777) && errno != EEXIST) {
        cerr << "unable to create output directory.\n"; perror("");
        assert(false && "unable to create output directory.");
    };

    sprintf(outfilepath, "%s/index.html", argv[2]);
    FILE *fout = fopen(outfilepath, "wb");
    fwrite(outbuf, 1, strlen(outbuf), fout);
    fclose(fout);
    
    return 0;
}
