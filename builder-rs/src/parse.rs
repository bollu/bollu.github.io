use crate::ast::*;

/// A parse error with location information.
#[derive(Clone, Debug)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.span, self.message)
    }
}

/// The parser state. Holds a reference to the input and tracks the current position.
struct Parser<'a> {
    input: &'a str,
    bytes: &'a [u8],
    loc: Loc,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            bytes: input.as_bytes(),
            loc: Loc::start(),
            errors: Vec::new(),
        }
    }

    // --- Low-level cursor operations ---

    fn at_end(&self) -> bool {
        self.loc.offset >= self.bytes.len()
    }

    fn peek(&self) -> Option<u8> {
        if self.at_end() { None } else { Some(self.bytes[self.loc.offset]) }
    }

    fn peek_at(&self, offset: usize) -> Option<u8> {
        self.bytes.get(offset).copied()
    }

    /// Advance one byte, updating line/col.
    fn advance(&mut self) -> u8 {
        let b = self.bytes[self.loc.offset];
        self.loc.offset += 1;
        if b == b'\n' {
            self.loc.line += 1;
            self.loc.col = 1;
        } else {
            self.loc.col += 1;
        }
        b
    }

    /// Check if input at current position starts with `prefix`.
    fn starts_with(&self, prefix: &str) -> bool {
        self.input[self.loc.offset..].starts_with(prefix)
    }

    /// Advance past `prefix` (which must be present at current position).
    fn skip(&mut self, prefix: &str) {
        for _ in prefix.bytes() {
            self.advance();
        }
    }

    fn error(&mut self, span: Span, message: impl Into<String>) {
        self.errors.push(ParseError { span, message: message.into() });
    }

    fn error_at(&mut self, loc: Loc, message: impl Into<String>) {
        self.errors.push(ParseError {
            span: Span::new(loc, loc),
            message: message.into(),
        });
    }

    /// Consume input until `delim` is found. Returns the span of the consumed content
    /// (not including the delimiter). Advances past the delimiter.
    /// Returns None if delimiter is not found before EOF.
    fn consume_until(&mut self, delim: &str) -> Option<Span> {
        let start = self.loc;
        while !self.at_end() {
            if self.starts_with(delim) {
                let end = self.loc;
                self.skip(delim);
                return Some(Span::new(start, end));
            }
            self.advance();
        }
        None
    }

    /// Consume whitespace (spaces and tabs) on the current line. Does not consume newlines.
    fn skip_intraline_whitespace(&mut self) {
        while let Some(b) = self.peek() {
            if b == b' ' || b == b'\t' {
                self.advance();
            } else {
                break;
            }
        }
    }

    // --- Block-level parsing ---

    /// Parse the entire input into a sequence of blocks.
    fn parse_blocks(&mut self) -> Vec<Block> {
        let mut blocks = Vec::new();
        while !self.at_end() {
            if let Some(block) = self.parse_block() {
                blocks.push(block);
            }
        }
        blocks
    }

    /// Parse a single block element at the current position.
    fn parse_block(&mut self) -> Option<Block> {
        if self.at_end() {
            return None;
        }

        let start = self.loc;

        // Two or more newlines → LineBreak
        if self.peek() == Some(b'\n') && self.peek_at(self.loc.offset + 1) == Some(b'\n') {
            while !self.at_end() {
                match self.peek() {
                    Some(b'\n') | Some(b' ') | Some(b'\t') => { self.advance(); }
                    _ => break,
                }
            }
            return Some(Block::LineBreak { span: Span::new(start, self.loc) });
        }

        // Single newline → skip and recurse (or treat as paragraph separator)
        if self.peek() == Some(b'\n') {
            self.advance();
            // Single newlines between inline content are handled within paragraph parsing.
            // At block level, a single newline after a block just separates blocks.
            return self.parse_block();
        }

        // $$ ... $$ → LatexBlock
        if self.starts_with("$$") {
            return Some(self.parse_latex_block(start));
        }

        // <script ... </script> → Html
        if self.starts_with("<script") {
            return Some(self.parse_html_script(start));
        }

        // <!-- ... --> → Comment
        if self.starts_with("<!--") {
            return Some(self.parse_comment(start));
        }

        // ``` ... ``` → CodeBlock
        if self.starts_with("```") {
            return Some(self.parse_code_block(start));
        }

        // # Heading
        if self.peek() == Some(b'#') {
            return Some(self.parse_heading(start));
        }

        // - list item
        if self.peek() == Some(b'-') && self.peek_at(self.loc.offset + 1).map_or(false, |b| b == b' ') {
            return Some(self.parse_unordered_list(start));
        }

        // Numbered list: 1. item
        if self.is_numbered_list_start() {
            return Some(self.parse_numbered_list(start));
        }

        // > quote
        if self.peek() == Some(b'>') {
            return Some(self.parse_quote(start));
        }

        // Otherwise: inline paragraph (one line of content)
        Some(self.parse_paragraph(start))
    }

    fn parse_latex_block(&mut self, start: Loc) -> Block {
        self.skip("$$");
        match self.consume_until("$$") {
            Some(content_span) => {
                let span = Span::new(start, self.loc);
                // Validate: must be followed by newline or EOF
                if !self.at_end() && self.peek() != Some(b'\n') {
                    self.error(span, "LaTeX block $$ must be followed by a newline");
                }
                let source = content_span.slice(self.input).to_string();
                Block::LatexBlock { source, span }
            }
            None => {
                let span = Span::new(start, self.loc);
                self.error(span, "unclosed $$ block (reached end of file)");
                Block::LatexBlock { source: String::new(), span }
            }
        }
    }

    fn parse_html_script(&mut self, start: Loc) -> Block {
        match self.consume_until("</script>") {
            Some(_) => {
                let span = Span::new(start, self.loc);
                let content = span.slice(self.input).to_string();
                Block::Html { content, span }
            }
            None => {
                let span = Span::new(start, self.loc);
                self.error(span, "unclosed <script> tag");
                Block::Html { content: span.slice(self.input).to_string(), span }
            }
        }
    }

    fn parse_comment(&mut self, start: Loc) -> Block {
        match self.consume_until("-->") {
            Some(_) => Block::Comment { span: Span::new(start, self.loc) },
            None => {
                let span = Span::new(start, self.loc);
                self.error(span, "unclosed comment <!-- ... -->");
                Block::Comment { span }
            }
        }
    }

    fn parse_code_block(&mut self, start: Loc) -> Block {
        self.skip("```");

        // Parse language name (until newline)
        let lang_start = self.loc;
        while !self.at_end() && self.peek() != Some(b'\n') {
            self.advance();
        }
        let lang = Span::new(lang_start, self.loc).slice(self.input).trim().to_string();

        // Skip the newline after lang
        if self.peek() == Some(b'\n') {
            self.advance();
        }

        // Consume until closing ```
        let code_start = self.loc;
        match self.consume_until("```") {
            Some(_content_span) => {
                let code = Span::new(code_start, Loc::new(
                    // The content_span.end points at where ``` starts, but consume_until
                    // already ate it. We need the content between code_start and right before ```.
                    self.loc.offset - 3, // back up past the ```
                    0, 0, // line/col don't matter for slicing
                )).slice(self.input).to_string();
                // Actually let's just use the span correctly
                let span = Span::new(start, self.loc);
                if !self.at_end() && self.peek() != Some(b'\n') {
                    self.error(span, "code block ``` must be followed by a newline");
                }
                Block::CodeBlock { lang, code, span }
            }
            None => {
                let span = Span::new(start, self.loc);
                self.error(span, "unclosed code block ```");
                let code = self.input[code_start.offset..].to_string();
                Block::CodeBlock { lang, code, span }
            }
        }
    }

    fn parse_heading(&mut self, start: Loc) -> Block {
        let mut level: u8 = 0;
        while self.peek() == Some(b'#') {
            self.advance();
            level += 1;
        }
        // Skip space after #
        self.skip_intraline_whitespace();

        let content = self.parse_inline_line();
        let span = if let Some(last) = content.last() {
            Span::new(start, last.span().end)
        } else {
            Span::new(start, self.loc)
        };

        Block::Heading { level, content, span }
    }

    fn parse_unordered_list(&mut self, start: Loc) -> Block {
        let mut items = Vec::new();
        while self.peek() == Some(b'-') {
            let item = self.parse_hyphen_list_item();
            items.push(item);
            // After the item, we should be at a newline or EOF
            if self.peek() == Some(b'\n') {
                self.advance();
            }
            // Continue if next line starts with -
            if self.peek() != Some(b'-') {
                break;
            }
            // Make sure it's actually "- " not just a hyphen in text
            if self.peek_at(self.loc.offset + 1).map_or(true, |b| b != b' ') {
                break;
            }
        }
        let span = if let Some(last) = items.last() {
            Span::new(start, last.span.end)
        } else {
            Span::new(start, self.loc)
        };
        Block::List { items, span }
    }

    fn parse_hyphen_list_item(&mut self) -> ListItem {
        let start = self.loc;
        assert_eq!(self.peek(), Some(b'-'));
        self.advance(); // skip -
        self.skip_intraline_whitespace();

        let mut content = Vec::new();
        loop {
            let line = self.parse_inline_line();
            content.extend(line);

            // We're now at newline or EOF
            if self.at_end() { break; }
            if self.peek() != Some(b'\n') { break; }

            // Check if next line is a continuation (starts with space)
            let next = self.peek_at(self.loc.offset + 1);
            match next {
                Some(b' ') | Some(b'\t') => {
                    // Continuation line - consume newline and leading whitespace
                    self.advance(); // consume \n
                    self.skip_intraline_whitespace();
                    if self.peek() == Some(b'\n') {
                        // Blank line after whitespace = end of item
                        break;
                    }
                    // Add a space to separate continuation lines
                    continue;
                }
                _ => break,
            }
        }

        let span = if let Some(last) = content.last() {
            Span::new(start, last.span().end)
        } else {
            Span::new(start, self.loc)
        };
        ListItem { content, span }
    }

    fn is_numbered_list_start(&self) -> bool {
        let mut off = self.loc.offset;
        while off < self.bytes.len() && self.bytes[off].is_ascii_digit() {
            off += 1;
        }
        // Made progress and next char is '.'
        off > self.loc.offset && off < self.bytes.len() && self.bytes[off] == b'.'
    }

    fn parse_numbered_list(&mut self, start: Loc) -> Block {
        let mut items = Vec::new();
        let mut expected_num = 1u64;
        while self.is_numbered_list_start() {
            let item_start = self.loc;
            // Parse and validate the number
            let mut num_str = String::new();
            while self.peek().map_or(false, |b| b.is_ascii_digit()) {
                num_str.push(self.advance() as char);
            }
            let num: u64 = num_str.parse().unwrap_or(0);
            if num != expected_num {
                self.error(Span::new(item_start, self.loc),
                    format!("expected list item number {}, found {}", expected_num, num));
            }
            expected_num += 1;

            // Skip the '.'
            if self.peek() == Some(b'.') {
                self.advance();
            }
            self.skip_intraline_whitespace();

            let mut content = Vec::new();
            loop {
                let line = self.parse_inline_line();
                content.extend(line);

                if self.at_end() { break; }
                if self.peek() != Some(b'\n') { break; }

                let next = self.peek_at(self.loc.offset + 1);
                match next {
                    Some(b' ') | Some(b'\t') => {
                        self.advance(); // consume \n
                        self.skip_intraline_whitespace();
                        if self.peek() == Some(b'\n') { break; }
                        continue;
                    }
                    _ => break,
                }
            }

            let span = if let Some(last) = content.last() {
                Span::new(item_start, last.span().end)
            } else {
                Span::new(item_start, self.loc)
            };
            items.push(ListItem { content, span });

            // Skip newline between items
            if self.peek() == Some(b'\n') {
                self.advance();
            }
        }
        let span = if let Some(last) = items.last() {
            Span::new(start, last.span.end)
        } else {
            Span::new(start, self.loc)
        };
        Block::NumberedList { items, span }
    }

    fn parse_quote(&mut self, start: Loc) -> Block {
        let mut lines = Vec::new();
        loop {
            assert_eq!(self.peek(), Some(b'>'));
            self.advance(); // skip >
            self.skip_intraline_whitespace();
            let line = self.parse_inline_line();
            // parse_inline_line consumes the trailing \n
            lines.push(line);

            // After parse_inline_line, we're at the start of the next line or EOF
            if self.at_end() { break; }

            // Continue if next line starts with >
            if self.peek() == Some(b'>') {
                continue;
            }
            break;
        }
        Block::Quote {
            span: Span::new(start, self.loc),
            lines,
        }
    }

    fn parse_paragraph(&mut self, start: Loc) -> Block {
        let content = self.parse_inline_line();
        let span = if let Some(last) = content.last() {
            Span::new(start, last.span().end)
        } else {
            Span::new(start, self.loc)
        };
        Block::Paragraph { content, span }
    }

    // --- Inline-level parsing ---

    /// Parse inline content until the end of the current line (newline or EOF).
    /// Does NOT consume the newline.
    fn parse_inline_line(&mut self) -> Vec<Inline> {
        let mut inlines = Vec::new();
        while !self.at_end() && self.peek() != Some(b'\n') {
            if let Some(inline) = self.parse_inline_fragment() {
                inlines.push(inline);
            } else {
                break;
            }
        }
        // Consume trailing newline if present
        if self.peek() == Some(b'\n') {
            self.advance();
        }
        inlines
    }

    /// Parse a single inline element.
    fn parse_inline_fragment(&mut self) -> Option<Inline> {
        if self.at_end() || self.peek() == Some(b'\n') {
            return None;
        }

        let start = self.loc;

        // Try link: [text](url)
        if self.peek() == Some(b'[') {
            if let Some(link) = self.try_parse_link(start) {
                return Some(link);
            }
            // Not a valid link — fall through to raw text
        }

        // Inline code: `..`
        if self.peek() == Some(b'`') {
            return Some(self.parse_inline_code(start));
        }

        // Inline math: $..$ (but not $$)
        if self.peek() == Some(b'$') && self.peek_at(self.loc.offset + 1) != Some(b'$') {
            return Some(self.parse_inline_latex(start));
        }

        // Bold: **..** or __..__ (check for double delimiter first)
        if (self.peek() == Some(b'*') && self.peek_at(self.loc.offset + 1) == Some(b'*'))
            || (self.peek() == Some(b'_') && self.peek_at(self.loc.offset + 1) == Some(b'_'))
        {
            return Some(self.parse_bold(start));
        }

        // Italic: *..*  or _.._ (single delimiter)
        if self.peek() == Some(b'*') || self.peek() == Some(b'_') {
            return Some(self.parse_italic(start));
        }

        // Raw text: consume until a special character or newline
        Some(self.parse_raw_text(start))
    }

    fn parse_inline_code(&mut self, start: Loc) -> Inline {
        assert_eq!(self.peek(), Some(b'`'));
        self.advance(); // skip opening `

        let content_start = self.loc;
        while !self.at_end() && self.peek() != Some(b'`') {
            if self.peek() == Some(b'\n') {
                self.error(
                    Span::new(start, self.loc),
                    "inline code block `...` must not span multiple lines",
                );
                break;
            }
            self.advance();
        }

        if self.peek() == Some(b'`') {
            self.advance(); // skip closing `
        } else if self.peek() != Some(b'\n') {
            self.error(Span::new(start, self.loc), "unclosed inline code `");
        }

        Inline::Code { span: Span::new(start, self.loc) }
    }

    fn parse_inline_latex(&mut self, start: Loc) -> Inline {
        assert_eq!(self.peek(), Some(b'$'));
        self.advance(); // skip opening $

        while !self.at_end() && self.peek() != Some(b'$') {
            if self.peek() == Some(b'\n') {
                self.error(
                    Span::new(start, self.loc),
                    "inline latex $...$ must not span multiple lines",
                );
                break;
            }
            self.advance();
        }

        if self.peek() == Some(b'$') {
            self.advance(); // skip closing $
        } else if self.peek() != Some(b'\n') {
            self.error(Span::new(start, self.loc), "unclosed inline latex $");
        }

        Inline::LatexInline { span: Span::new(start, self.loc) }
    }

    fn parse_bold(&mut self, start: Loc) -> Inline {
        let delim = self.peek().unwrap();
        self.advance(); // first delim char
        self.advance(); // second delim char

        let mut content = Vec::new();
        loop {
            if self.at_end() || self.peek() == Some(b'\n') {
                self.error(Span::new(start, self.loc), "unclosed bold delimiter");
                break;
            }
            // Check for closing **  or __
            if self.peek() == Some(delim) && self.peek_at(self.loc.offset + 1) == Some(delim) {
                self.advance();
                self.advance();
                break;
            }
            if let Some(frag) = self.parse_inline_fragment() {
                content.push(frag);
            } else {
                break;
            }
        }

        Inline::Bold { content, span: Span::new(start, self.loc) }
    }

    fn parse_italic(&mut self, start: Loc) -> Inline {
        let delim = self.peek().unwrap();
        self.advance(); // skip opening delimiter

        let mut content = Vec::new();
        loop {
            if self.at_end() || self.peek() == Some(b'\n') {
                self.error(Span::new(start, self.loc), "unclosed italic delimiter");
                break;
            }
            if self.peek() == Some(delim) {
                // Make sure it's not ** (bold closing misinterpreted)
                self.advance(); // consume closing delimiter
                break;
            }
            if let Some(frag) = self.parse_inline_fragment() {
                content.push(frag);
            } else {
                break;
            }
        }

        Inline::Italic { content, span: Span::new(start, self.loc) }
    }

    fn try_parse_link(&mut self, start: Loc) -> Option<Inline> {
        // Save state so we can backtrack
        let saved_loc = self.loc;

        assert_eq!(self.peek(), Some(b'['));
        self.advance(); // skip [

        // Parse link text (inline fragments until ])
        let mut text = Vec::new();
        loop {
            if self.at_end() || self.peek() == Some(b'\n') {
                // Not a valid link — backtrack
                self.loc = saved_loc;
                return None;
            }
            if self.peek() == Some(b']') {
                self.advance(); // skip ]
                break;
            }
            if let Some(frag) = self.parse_inline_fragment() {
                text.push(frag);
            } else {
                self.loc = saved_loc;
                return None;
            }
        }

        // Must be followed by (
        if self.peek() != Some(b'(') {
            self.loc = saved_loc;
            return None;
        }
        self.advance(); // skip (

        // Parse URL until )
        let url_start = self.loc;
        while !self.at_end() && self.peek() != Some(b')') && self.peek() != Some(b'\n') {
            self.advance();
        }
        let url = Span::new(url_start, self.loc).slice(self.input).to_string();

        if self.peek() == Some(b')') {
            self.advance(); // skip )
        } else {
            self.loc = saved_loc;
            return None;
        }

        Some(Inline::Link {
            text,
            url,
            span: Span::new(start, self.loc),
        })
    }

    fn parse_raw_text(&mut self, start: Loc) -> Inline {
        while !self.at_end() {
            match self.peek() {
                Some(b'*') | Some(b'[') | Some(b']') | Some(b'$') |
                Some(b'`') | Some(b'_') | Some(b'\n') => break,
                _ => { self.advance(); }
            }
        }
        Inline::Text { span: Span::new(start, self.loc) }
    }
}

/// Parse the input string into a list of blocks.
/// Returns Ok(blocks) on success, or Err(errors) if any parse errors were found.
/// Note: the parser is error-recovering, so even on Err, partial results may be useful.
/// For now, we return all blocks even when there are errors, with the errors alongside.
pub fn parse(input: &str) -> (Vec<Block>, Vec<ParseError>) {
    let mut parser = Parser::new(input);
    let blocks = parser.parse_blocks();
    (blocks, parser.errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_ok(input: &str) -> Vec<Block> {
        let (blocks, errors) = parse(input);
        if !errors.is_empty() {
            for e in &errors {
                eprintln!("parse error: {}", e);
            }
        }
        assert!(errors.is_empty(), "unexpected parse errors");
        blocks
    }

    #[test]
    fn test_heading() {
        let blocks = parse_ok("# Hello world\n");
        assert_eq!(blocks.len(), 1);
        match &blocks[0] {
            Block::Heading { level, content, .. } => {
                assert_eq!(*level, 1);
                assert_eq!(content.len(), 1);
            }
            other => panic!("expected Heading, got {:?}", other),
        }
    }

    #[test]
    fn test_code_block() {
        let blocks = parse_ok("```rust\nfn main() {}\n```\n");
        assert_eq!(blocks.len(), 1);
        match &blocks[0] {
            Block::CodeBlock { lang, code, .. } => {
                assert_eq!(lang, "rust");
                assert_eq!(code, "fn main() {}\n");
            }
            other => panic!("expected CodeBlock, got {:?}", other),
        }
    }

    #[test]
    fn test_latex_block() {
        let blocks = parse_ok("$$x + y$$\n");
        assert_eq!(blocks.len(), 1);
        match &blocks[0] {
            Block::LatexBlock { source, .. } => {
                assert_eq!(source, "x + y");
            }
            other => panic!("expected LatexBlock, got {:?}", other),
        }
    }

    #[test]
    fn test_inline_code() {
        let blocks = parse_ok("hello `world` end\n");
        assert_eq!(blocks.len(), 1);
        match &blocks[0] {
            Block::Paragraph { content, .. } => {
                assert_eq!(content.len(), 3);
                assert!(matches!(&content[0], Inline::Text { .. }));
                assert!(matches!(&content[1], Inline::Code { .. }));
                assert!(matches!(&content[2], Inline::Text { .. }));
            }
            other => panic!("expected Paragraph, got {:?}", other),
        }
    }

    #[test]
    fn test_link() {
        let blocks = parse_ok("[click here](https://example.com)\n");
        assert_eq!(blocks.len(), 1);
        match &blocks[0] {
            Block::Paragraph { content, .. } => {
                assert_eq!(content.len(), 1);
                match &content[0] {
                    Inline::Link { url, text, .. } => {
                        assert_eq!(url, "https://example.com");
                        assert_eq!(text.len(), 1);
                    }
                    other => panic!("expected Link, got {:?}", other),
                }
            }
            other => panic!("expected Paragraph, got {:?}", other),
        }
    }

    #[test]
    fn test_bold_and_italic() {
        let blocks = parse_ok("**bold** and *italic*\n");
        assert_eq!(blocks.len(), 1);
        match &blocks[0] {
            Block::Paragraph { content, .. } => {
                assert!(matches!(&content[0], Inline::Bold { .. }));
                assert!(matches!(&content[1], Inline::Text { .. }));
                assert!(matches!(&content[2], Inline::Italic { .. }));
            }
            other => panic!("expected Paragraph, got {:?}", other),
        }
    }

    #[test]
    fn test_unordered_list() {
        let blocks = parse_ok("- item one\n- item two\n");
        assert_eq!(blocks.len(), 1);
        match &blocks[0] {
            Block::List { items, .. } => {
                assert_eq!(items.len(), 2);
            }
            other => panic!("expected List, got {:?}", other),
        }
    }

    #[test]
    fn test_numbered_list() {
        let blocks = parse_ok("1. first\n2. second\n");
        assert_eq!(blocks.len(), 1);
        match &blocks[0] {
            Block::NumberedList { items, .. } => {
                assert_eq!(items.len(), 2);
            }
            other => panic!("expected NumberedList, got {:?}", other),
        }
    }

    #[test]
    fn test_quote() {
        let blocks = parse_ok("> hello\n> world\n");
        assert_eq!(blocks.len(), 1);
        match &blocks[0] {
            Block::Quote { lines, .. } => {
                assert_eq!(lines.len(), 2);
            }
            other => panic!("expected Quote, got {:?}", other),
        }
    }

    #[test]
    fn test_comment() {
        let blocks = parse_ok("<!-- this is a comment -->\n");
        assert_eq!(blocks.len(), 1);
        assert!(matches!(&blocks[0], Block::Comment { .. }));
    }

    #[test]
    fn test_line_break() {
        let blocks = parse_ok("hello\n\n\nworld\n");
        // Should get: Paragraph("hello"), LineBreak, Paragraph("world")
        assert!(blocks.len() >= 2);
    }

    #[test]
    fn test_mixed_content() {
        let input = "# Title\n\nSome text with `code` and $math$\n\n- item 1\n- item 2\n";
        let blocks = parse_ok(input);
        assert!(blocks.len() >= 3);
        assert!(matches!(&blocks[0], Block::Heading { level: 1, .. }));
    }
}
