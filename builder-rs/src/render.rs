use crate::ast::*;

/// Trait for syntax-highlighting code blocks.
pub trait CodeHighlighter: Send + Sync {
    fn highlight(&self, code: &str, lang: &str) -> Result<String, String>;
}

/// Trait for compiling LaTeX to HTML.
pub trait LatexCompiler: Send + Sync {
    fn render(&self, source: &str, display_mode: bool) -> Result<String, String>;
}

/// A no-op highlighter that returns the code as-is (HTML-escaped).
pub struct PassthroughHighlighter;

impl CodeHighlighter for PassthroughHighlighter {
    fn highlight(&self, code: &str, _lang: &str) -> Result<String, String> {
        Ok(html_escape(code))
    }
}

/// A no-op LaTeX compiler that returns the source wrapped in a span.
pub struct PassthroughLatex;

impl LatexCompiler for PassthroughLatex {
    fn render(&self, source: &str, display_mode: bool) -> Result<String, String> {
        if display_mode {
            Ok(format!("<span class='latex-fallback'>$${}$$</span>", html_escape(source)))
        } else {
            Ok(format!("<span class='latex-fallback'>${}$</span>", html_escape(source)))
        }
    }
}

pub fn html_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(c),
        }
    }
    out
}

/// Extract plain text from inline elements (stripping all markup).
pub fn inlines_to_plaintext(inlines: &[Inline], input: &str) -> String {
    let mut out = String::new();
    for inline in inlines {
        inline_to_plaintext(inline, input, &mut out);
    }
    out
}

fn inline_to_plaintext(inline: &Inline, input: &str, out: &mut String) {
    match inline {
        Inline::Text { span } => {
            out.push_str(span.slice(input));
        }
        Inline::Code { span } => {
            // Strip the backticks
            let s = span.slice(input);
            if s.starts_with('`') && s.ends_with('`') && s.len() >= 2 {
                out.push_str(&s[1..s.len()-1]);
            } else {
                out.push_str(s);
            }
        }
        Inline::LatexInline { span } => {
            let s = span.slice(input);
            if s.starts_with('$') && s.ends_with('$') && s.len() >= 2 {
                out.push_str(&s[1..s.len()-1]);
            } else {
                out.push_str(s);
            }
        }
        Inline::Bold { content, .. } => {
            for inner in content {
                inline_to_plaintext(inner, input, out);
            }
        }
        Inline::Italic { content, .. } => {
            for inner in content {
                inline_to_plaintext(inner, input, out);
            }
        }
        Inline::Link { text, .. } => {
            for inner in text {
                inline_to_plaintext(inner, input, out);
            }
        }
    }
}

/// Convert a heading's inline content into a URL slug (GitHub-flavored).
///
/// Rules:
/// - Lowercase all letters
/// - Keep digits
/// - Convert spaces/hyphens to single hyphens
/// - Remove everything else
pub fn heading_to_slug(inlines: &[Inline], input: &str) -> String {
    let plaintext = inlines_to_plaintext(inlines, input);
    let trimmed = plaintext.trim();

    let mut slug = String::new();
    let mut seen_alnum = true; // don't start with a hyphen

    for c in trimmed.chars() {
        if c.is_alphanumeric() {
            if !seen_alnum && !slug.is_empty() {
                slug.push('-');
            }
            seen_alnum = true;
            if c.is_alphabetic() {
                for lc in c.to_lowercase() {
                    slug.push(lc);
                }
            } else {
                slug.push(c);
            }
        } else if c == '-' {
            seen_alnum = false;
        } else if c.is_whitespace() {
            seen_alnum = false;
        }
        // else: skip the character
    }

    slug
}

/// Configuration for the HTML output.
pub struct RenderConfig {
    pub articles_url_prefix: String,
}

impl Default for RenderConfig {
    fn default() -> Self {
        Self {
            articles_url_prefix: "/articles/".to_string(),
        }
    }
}

/// Render a sequence of blocks to an HTML fragment string.
pub fn render_blocks(
    blocks: &[Block],
    input: &str,
    highlighter: &dyn CodeHighlighter,
    latex: &dyn LatexCompiler,
) -> String {
    let mut out = String::new();
    for block in blocks {
        render_block(&mut out, block, input, highlighter, latex);
    }
    out
}

fn render_block(
    out: &mut String,
    block: &Block,
    input: &str,
    hl: &dyn CodeHighlighter,
    latex: &dyn LatexCompiler,
) {
    match block {
        Block::Comment { .. } => { /* skip */ }

        Block::LineBreak { .. } => {
            out.push_str("<br/>");
        }

        Block::Html { content, .. } => {
            out.push_str(content);
            out.push(' ');
        }

        Block::Paragraph { content, .. } => {
            out.push_str("<span class='centered'>");
            render_inlines(out, content, input, hl, latex);
            out.push_str("</span>");
        }

        Block::CodeBlock { lang, code, span } => {
            if lang == "abc" {
                out.push_str("<div class=\"abc\">");
                out.push_str(code);
                out.push_str("</div>");
            } else {
                out.push_str("<pre><code>");
                match hl.highlight(code, lang) {
                    Ok(highlighted) => out.push_str(&highlighted),
                    Err(e) => {
                        eprintln!("warning: syntax highlight failed at {}: {}", span, e);
                        out.push_str(&html_escape(code));
                    }
                }
                out.push_str("</code></pre>");
            }
        }

        Block::LatexBlock { source, span } => {
            out.push_str("<div class='latexblock'>");
            match latex.render(source, true) {
                Ok(rendered) => out.push_str(&rendered),
                Err(e) => {
                    eprintln!("warning: latex compilation failed at {}: {}", span, e);
                    out.push_str(&html_escape(source));
                }
            }
            out.push_str("</div>");
        }

        Block::List { items, .. } => {
            out.push_str("<ul>");
            for item in items {
                out.push_str("<li>");
                render_inlines(out, &item.content, input, hl, latex);
                out.push_str("</li>");
            }
            out.push_str("</ul>");
        }

        Block::NumberedList { items, .. } => {
            out.push_str("<ol>");
            for item in items {
                out.push_str("<li>");
                render_inlines(out, &item.content, input, hl, latex);
                out.push_str("</li>");
            }
            out.push_str("</ol>");
        }

        Block::Heading { level, content, .. } => {
            let slug = heading_to_slug(content, input);
            let html_level = std::cmp::min(4, 1 + *level as u32);
            out.push_str(&format!("<h{}>", html_level));
            out.push_str(&format!("<a id='{}' href='#{}'>", slug, slug));
            out.push_str(" \u{00a7} ");
            out.push_str("</a>");
            render_inlines(out, content, input, hl, latex);
            out.push_str(&format!("</h{}>", html_level));
        }

        Block::Quote { lines, .. } => {
            out.push_str("<blockquote>");
            for line in lines {
                render_inlines(out, line, input, hl, latex);
                out.push_str("<br/>");
            }
            out.push_str("</blockquote>");
        }
    }
}

fn render_inlines(
    out: &mut String,
    inlines: &[Inline],
    input: &str,
    hl: &dyn CodeHighlighter,
    latex: &dyn LatexCompiler,
) {
    for inline in inlines {
        render_inline(out, inline, input, hl, latex);
    }
}

fn render_inline(
    out: &mut String,
    inline: &Inline,
    input: &str,
    hl: &dyn CodeHighlighter,
    latex: &dyn LatexCompiler,
) {
    match inline {
        Inline::Text { span } => {
            out.push_str(span.slice(input));
            out.push(' ');
        }

        Inline::Code { span } => {
            let s = span.slice(input);
            let inner = if s.starts_with('`') && s.ends_with('`') && s.len() >= 2 {
                &s[1..s.len()-1]
            } else {
                s
            };
            out.push_str("<code class='inline'>");
            out.push_str(inner);
            out.push_str("</code>");
        }

        Inline::LatexInline { span } => {
            let s = span.slice(input);
            let inner = if s.starts_with('$') && s.ends_with('$') && s.len() >= 2 {
                &s[1..s.len()-1]
            } else {
                s
            };
            out.push_str("<span class='latexinline'>");
            match latex.render(inner, false) {
                Ok(rendered) => out.push_str(&rendered),
                Err(e) => {
                    eprintln!("warning: inline latex failed at {}: {}", span, e);
                    out.push_str(&html_escape(inner));
                }
            }
            out.push_str("</span>");
        }

        Inline::Bold { content, .. } => {
            out.push_str("<b>");
            render_inlines(out, content, input, hl, latex);
            out.push_str("</b>");
        }

        Inline::Italic { content, .. } => {
            out.push_str("<i>");
            render_inlines(out, content, input, hl, latex);
            out.push_str("</i>");
        }

        Inline::Link { text, url, .. } => {
            out.push_str(&format!("<a href={}>", url));
            render_inlines(out, text, input, hl, latex);
            out.push_str("</a>");
        }
    }
}

/// Render an RSS-safe title from a heading's inline content.
pub fn heading_to_rss_title(inlines: &[Inline], input: &str) -> String {
    let mut out = String::new();
    for inline in inlines {
        rss_inline(inline, input, &mut out);
    }
    out
}

fn rss_escape_char(c: char, out: &mut String) {
    match c {
        '<' => out.push_str("&lt;"),
        '>' => out.push_str("&gt;"),
        '"' => out.push_str("&quot;"),
        '\'' => out.push_str("&apos;"),
        _ => out.push(c),
    }
}

fn rss_inline(inline: &Inline, input: &str, out: &mut String) {
    match inline {
        Inline::Text { span } => {
            for c in span.slice(input).chars() {
                rss_escape_char(c, out);
            }
            out.push(' ');
        }
        Inline::Code { span } | Inline::LatexInline { span } => {
            let s = span.slice(input);
            // Strip delimiters
            let inner = if s.len() >= 2 { &s[1..s.len()-1] } else { s };
            for c in inner.chars() {
                rss_escape_char(c, out);
            }
        }
        Inline::Bold { content, .. } | Inline::Italic { content, .. } => {
            for inner in content {
                rss_inline(inner, input, out);
            }
        }
        Inline::Link { text, .. } => {
            for inner in text {
                rss_inline(inner, input, out);
            }
        }
    }
}

/// The HTML preamble for each page.
pub fn html_preamble() -> &'static str {
    concat!(
        "<!DOCTYPE html>",
        "<meta charset='UTF-8'>",
        "<html>",
        "<head>",
        "<meta name='viewport' content='width=device-width, initial-scale=1'>",
        "<script src='/abcjs/abcjs-basic-min.js'></script>",
        "<link rel='stylesheet' href='/abcjs/abcjs-audio.css' >",
        "<link rel='alternate' type='application/rss+xml' href='feed.rss' title='A universe of sorts'/>",
        "<link rel='stylesheet' href='/katex/katex.min.css'",
        "    integrity='sha384-AfEj0r4/OFrOo5t7NnNe46zW/tFgW6x/bCJG8FqQCEo3+Aro6EYUG4+cU+KJWu/X'",
        "    crossorigin='anonymous'>",
        "<!-- The loading of KaTeX is deferred to speed up page rendering -->",
        "<link rel='stylesheet' href='/prism/prism.css'>",
        "<title> A Universe of Sorts </title>",
        "<link rel='stylesheet' href='/css/stylesheet.css'>",
        "<script src='/script/blog.js'></script>",
        "</head>",
        "<body>",
        "<div class='container'>",
    )
}

/// The HTML postamble for each page.
pub fn html_postamble() -> &'static str {
    "</container></body></html>"
}

/// The utterances comment widget.
pub fn utterances_widget() -> &'static str {
    concat!(
        "<script src=\"https://utteranc.es/client.js\"",
        "        repo=\"bollu/bollu.github.io\"",
        "        issue-term=\"pathname\"",
        "        label=\"question\"",
        "        theme=\"github-light\"",
        "        crossorigin=\"anonymous\"",
        "        async>",
        "</script>",
    )
}
