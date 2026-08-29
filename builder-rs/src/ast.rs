use std::fmt;

/// Source location in the input file.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    /// Byte offset from the start of the input.
    pub offset: usize,
    /// 1-based line number.
    pub line: u32,
    /// 1-based column number.
    pub col: u32,
}

impl Loc {
    pub fn new(offset: usize, line: u32, col: u32) -> Self {
        Self { offset, line, col }
    }

    /// The start of a file.
    pub fn start() -> Self {
        Self { offset: 0, line: 1, col: 1 }
    }
}

impl fmt::Debug for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Half-open byte range [start, end) in the input.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

impl Span {
    pub fn new(start: Loc, end: Loc) -> Self {
        debug_assert!(end.offset >= start.offset);
        Self { start, end }
    }

    pub fn len(&self) -> usize {
        self.end.offset - self.start.offset
    }

    /// Extract the spanned slice from the input.
    pub fn slice<'a>(&self, input: &'a str) -> &'a str {
        &input[self.start.offset..self.end.offset]
    }

    /// Merge two spans into one covering both.
    pub fn merge(self, other: Span) -> Span {
        let start = if self.start.offset <= other.start.offset { self.start } else { other.start };
        let end = if self.end.offset >= other.end.offset { self.end } else { other.end };
        Span { start, end }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}-{}]", self.start, self.end)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.start.line, self.start.col)
    }
}

/// Top-level block elements of the document.
#[derive(Clone, Debug)]
pub enum Block {
    /// `# Heading` through `##### Heading`. Level is 1-5.
    Heading {
        level: u8,
        content: Vec<Inline>,
        span: Span,
    },
    /// A paragraph of inline content (one or more lines not matching any other block rule).
    Paragraph {
        content: Vec<Inline>,
        span: Span,
    },
    /// Fenced code block: ```lang ... ```
    CodeBlock {
        lang: String,
        code: String,
        span: Span,
    },
    /// Display math: $$ ... $$
    LatexBlock {
        source: String,
        span: Span,
    },
    /// Unordered list: - item
    List {
        items: Vec<ListItem>,
        span: Span,
    },
    /// Ordered list: 1. item, 2. item, ...
    NumberedList {
        items: Vec<ListItem>,
        span: Span,
    },
    /// Block quote: > text
    Quote {
        lines: Vec<Vec<Inline>>,
        span: Span,
    },
    /// Raw HTML: <script>...</script>
    Html {
        content: String,
        span: Span,
    },
    /// HTML comment: <!-- ... -->
    Comment {
        span: Span,
    },
    /// Two or more consecutive newlines.
    LineBreak {
        span: Span,
    },
}

/// A list item is a sequence of inline content (possibly spanning continuation lines).
#[derive(Clone, Debug)]
pub struct ListItem {
    pub content: Vec<Inline>,
    pub span: Span,
}

/// Inline elements within a block.
#[derive(Clone, Debug)]
pub enum Inline {
    /// Plain text.
    Text {
        span: Span,
    },
    /// Inline code: `code`
    Code {
        span: Span,
    },
    /// Inline math: $source$
    LatexInline {
        span: Span,
    },
    /// **bold** or __bold__
    Bold {
        content: Vec<Inline>,
        span: Span,
    },
    /// *italic* or _italic_
    Italic {
        content: Vec<Inline>,
        span: Span,
    },
    /// [text](url)
    Link {
        text: Vec<Inline>,
        url: String,
        span: Span,
    },
}

impl Block {
    pub fn span(&self) -> Span {
        match self {
            Block::Heading { span, .. }
            | Block::Paragraph { span, .. }
            | Block::CodeBlock { span, .. }
            | Block::LatexBlock { span, .. }
            | Block::List { span, .. }
            | Block::NumberedList { span, .. }
            | Block::Quote { span, .. }
            | Block::Html { span, .. }
            | Block::Comment { span, .. }
            | Block::LineBreak { span, .. } => *span,
        }
    }
}

impl Inline {
    pub fn span(&self) -> Span {
        match self {
            Inline::Text { span, .. }
            | Inline::Code { span, .. }
            | Inline::LatexInline { span, .. }
            | Inline::Bold { span, .. }
            | Inline::Italic { span, .. }
            | Inline::Link { span, .. } => *span,
        }
    }
}

/// Pretty-print the AST for debugging.
pub fn dump_ast(blocks: &[Block], input: &str) -> String {
    let mut out = String::new();
    for block in blocks {
        dump_block(&mut out, block, input, 0);
    }
    out
}

fn indent(out: &mut String, level: usize) {
    for _ in 0..level {
        out.push_str("  ");
    }
}

fn dump_block(out: &mut String, block: &Block, input: &str, depth: usize) {
    indent(out, depth);
    match block {
        Block::Heading { level, content, span } => {
            out.push_str(&format!("Heading(h{}) {} {{\n", level, span));
            for inline in content {
                dump_inline(out, inline, input, depth + 1);
            }
            indent(out, depth);
            out.push_str("}\n");
        }
        Block::Paragraph { content, span } => {
            out.push_str(&format!("Paragraph {} {{\n", span));
            for inline in content {
                dump_inline(out, inline, input, depth + 1);
            }
            indent(out, depth);
            out.push_str("}\n");
        }
        Block::CodeBlock { lang, span, .. } => {
            out.push_str(&format!("CodeBlock(lang={:?}) {}\n", lang, span));
        }
        Block::LatexBlock { span, .. } => {
            out.push_str(&format!("LatexBlock {}\n", span));
        }
        Block::List { items, span } => {
            out.push_str(&format!("List {} {{\n", span));
            for item in items {
                indent(out, depth + 1);
                out.push_str("- ");
                for inline in &item.content {
                    dump_inline(out, inline, input, depth + 2);
                }
            }
            indent(out, depth);
            out.push_str("}\n");
        }
        Block::NumberedList { items, span } => {
            out.push_str(&format!("NumberedList {} {{\n", span));
            for (i, item) in items.iter().enumerate() {
                indent(out, depth + 1);
                out.push_str(&format!("{}. ", i + 1));
                for inline in &item.content {
                    dump_inline(out, inline, input, depth + 2);
                }
            }
            indent(out, depth);
            out.push_str("}\n");
        }
        Block::Quote { lines, span } => {
            out.push_str(&format!("Quote {} {{\n", span));
            for line in lines {
                indent(out, depth + 1);
                out.push_str("> ");
                for inline in line {
                    dump_inline(out, inline, input, depth + 2);
                }
            }
            indent(out, depth);
            out.push_str("}\n");
        }
        Block::Html { span, .. } => {
            out.push_str(&format!("Html {}\n", span));
        }
        Block::Comment { span } => {
            out.push_str(&format!("Comment {}\n", span));
        }
        Block::LineBreak { span } => {
            out.push_str(&format!("LineBreak {}\n", span));
        }
    }
}

fn dump_inline(out: &mut String, inline: &Inline, input: &str, depth: usize) {
    indent(out, depth);
    match inline {
        Inline::Text { span } => {
            out.push_str(&format!("Text {:?}\n", span.slice(input)));
        }
        Inline::Code { span } => {
            out.push_str(&format!("Code {:?}\n", span.slice(input)));
        }
        Inline::LatexInline { span } => {
            out.push_str(&format!("LatexInline {:?}\n", span.slice(input)));
        }
        Inline::Bold { content, .. } => {
            out.push_str("Bold {\n");
            for inner in content {
                dump_inline(out, inner, input, depth + 1);
            }
            indent(out, depth);
            out.push_str("}\n");
        }
        Inline::Italic { content, .. } => {
            out.push_str("Italic {\n");
            for inner in content {
                dump_inline(out, inner, input, depth + 1);
            }
            indent(out, depth);
            out.push_str("}\n");
        }
        Inline::Link { text, url, .. } => {
            out.push_str(&format!("Link(url={:?}) {{\n", url));
            for inner in text {
                dump_inline(out, inner, input, depth + 1);
            }
            indent(out, depth);
            out.push_str("}\n");
        }
    }
}
