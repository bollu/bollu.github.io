mod ast;
mod js;
mod parse;
mod render;
mod rss;

use crate::ast::*;
use crate::render::*;
use std::fs;
use std::path::PathBuf;

/// An article is a section of the blog delimited by H1 headings.
struct Article<'a> {
    blocks: &'a [Block],
    slug: String,
    index: usize,
}

/// Split blocks into articles by H1 headings.
/// Returns (preamble_blocks, articles) where preamble is everything before the first H1.
fn split_articles<'a>(blocks: &'a [Block], input: &str) -> (&'a [Block], Vec<Article<'a>>) {
    // Find all H1 indices
    let h1_indices: Vec<usize> = blocks.iter().enumerate()
        .filter_map(|(i, b)| {
            if let Block::Heading { level: 1, .. } = b { Some(i) } else { None }
        })
        .collect();

    if h1_indices.is_empty() {
        return (blocks, Vec::new());
    }

    let preamble = &blocks[..h1_indices[0]];

    let mut articles = Vec::new();
    for (idx, &start) in h1_indices.iter().enumerate() {
        let end = h1_indices.get(idx + 1).copied().unwrap_or(blocks.len());
        let article_blocks = &blocks[start..end];

        let slug = if let Block::Heading { content, .. } = &blocks[start] {
            heading_to_slug(content, input)
        } else {
            unreachable!()
        };

        articles.push(Article {
            blocks: article_blocks,
            slug,
            index: idx,
        });
    }

    (preamble, articles)
}

/// Generate the table of contents HTML from the list of articles.
fn render_toc(articles: &[Article], input: &str) -> String {
    let mut out = String::new();
    out.push_str("<ol reversed>");
    for article in articles {
        if let Block::Heading { content, .. } = &article.blocks[0] {
            let slug = &article.slug;
            out.push_str(&format!("<li><a href='/articles/{}.html'>", slug));
            // Render heading content as inline HTML (simple text for TOC)
            let pt = PassthroughHighlighter;
            let lt = PassthroughLatex;
            render_inlines_to(&mut out, content, input, &pt, &lt);
            out.push_str("</a></li>");
        }
    }
    out.push_str("</ol>");
    out
}

/// Helper to render inlines (re-exports the internal function for TOC use).
fn render_inlines_to(
    out: &mut String,
    inlines: &[Inline],
    input: &str,
    hl: &dyn CodeHighlighter,
    latex: &dyn LatexCompiler,
) {
    // We need to call the render module's inline rendering.
    // Since render_blocks works on blocks, we construct a temporary paragraph.
    for inline in inlines {
        render_single_inline(out, inline, input, hl, latex);
    }
}

fn render_single_inline(
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
            if let Ok(rendered) = latex.render(inner, false) {
                out.push_str(&rendered);
            } else {
                out.push_str(inner);
            }
            out.push_str("</span>");
        }
        Inline::Bold { content, .. } => {
            out.push_str("<b>");
            for inner in content {
                render_single_inline(out, inner, input, hl, latex);
            }
            out.push_str("</b>");
        }
        Inline::Italic { content, .. } => {
            out.push_str("<i>");
            for inner in content {
                render_single_inline(out, inner, input, hl, latex);
            }
            out.push_str("</i>");
        }
        Inline::Link { text, url, .. } => {
            out.push_str(&format!("<a href={}>", url));
            for inner in text {
                render_single_inline(out, inner, input, hl, latex);
            }
            out.push_str("</a>");
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let blog_root = if args.len() > 1 {
        PathBuf::from(&args[1])
    } else {
        eprintln!("Usage: builder <path-to-blog-root>");
        eprintln!("  The blog root should contain README.txt, katex/, and prism/");
        std::process::exit(1);
    };

    let input_path = blog_root.join("README.txt");
    let katex_path = blog_root.join("katex").join("katex.min.js");
    let prism_path = blog_root.join("prism").join("prism.js");
    let output_dir = blog_root.join("out");
    let articles_dir = output_dir.join("articles");

    // Read input
    eprintln!("=== Reading {} ===", input_path.display());
    let input = fs::read_to_string(&input_path)
        .unwrap_or_else(|e| {
            eprintln!("Error: cannot read {}: {}", input_path.display(), e);
            std::process::exit(1);
        });
    eprintln!("=== Input length: {} bytes ===", input.len());

    // Parse
    eprintln!("=== Parsing... ===");
    let (blocks, errors) = parse::parse(&input);
    if !errors.is_empty() {
        eprintln!("=== {} parse error(s): ===", errors.len());
        for err in &errors {
            eprintln!("  {}", err);
        }
    }
    eprintln!("=== Parsed {} blocks ===", blocks.len());

    // Initialize JS engines
    eprintln!("=== Loading KaTeX... ===");
    let latex: Box<dyn LatexCompiler> = match js::QuickJsKatex::new(katex_path.to_str().unwrap()) {
        Ok(k) => Box::new(k),
        Err(e) => {
            eprintln!("Warning: failed to load KaTeX: {}. Using passthrough.", e);
            Box::new(PassthroughLatex)
        }
    };

    eprintln!("=== Loading Prism.js... ===");
    let highlighter: Box<dyn CodeHighlighter> = match js::QuickJsPrism::new(prism_path.to_str().unwrap()) {
        Ok(p) => Box::new(p),
        Err(e) => {
            eprintln!("Warning: failed to load Prism.js: {}. Using passthrough.", e);
            Box::new(PassthroughHighlighter)
        }
    };

    // Create output directories
    fs::create_dir_all(&articles_dir).unwrap_or_else(|e| {
        eprintln!("Error: cannot create {}: {}", articles_dir.display(), e);
        std::process::exit(1);
    });

    // Split into articles
    let (preamble, articles) = split_articles(&blocks, &input);

    // Write index.html
    eprintln!("=== Writing index.html ===");
    {
        let mut index_html = String::new();
        index_html.push_str(html_preamble());
        index_html.push_str(&render_blocks(preamble, &input, highlighter.as_ref(), latex.as_ref()));
        index_html.push_str(&render_toc(&articles, &input));
        index_html.push_str(html_postamble());

        let index_path = output_dir.join("index.html");
        fs::write(&index_path, &index_html)
            .unwrap_or_else(|e| {
                eprintln!("Error writing {}: {}", index_path.display(), e);
                std::process::exit(1);
            });
    }

    // Render each article (this is the parallelizable part)
    eprintln!("=== Writing {} articles ===", articles.len());
    let article_results: Vec<(String, String)> = articles.iter().map(|article| {
        let mut html = String::new();
        html.push_str(html_preamble());
        html.push_str(&render_blocks(article.blocks, &input, highlighter.as_ref(), latex.as_ref()));

        // Footer navigation
        html.push_str("<div id=\"footer\">");
        if article.index > 0 {
            let prev = &articles[article.index - 1];
            html.push_str(&format!(
                "<a class=\"footer-item\" href=\"/articles/{}.html\"> Newer </a>",
                prev.slug
            ));
        }
        html.push_str("  \u{09E8} <a href=\"/\" class=\"footer-item\"> Blog </a>  \u{09E8}  ");
        if article.index + 1 < articles.len() {
            let next = &articles[article.index + 1];
            html.push_str(&format!(
                "<a class=\"footer-item\" href=\"/articles/{}.html\"> Older </a>",
                next.slug
            ));
        }
        html.push_str("</div>");
        html.push_str(utterances_widget());
        html.push_str(html_postamble());

        (article.slug.clone(), html)
    }).collect();

    for (slug, html) in &article_results {
        let path = output_dir.join(format!("articles/{}.html", slug));
        eprintln!("  Writing {}", path.display());
        fs::write(&path, html).unwrap_or_else(|e| {
            eprintln!("Error writing {}: {}", path.display(), e);
        });
    }

    // Write RSS feed
    eprintln!("=== Writing feed.rss ===");
    let rss_entries = rss::extract_rss_entries(&blocks, &input);
    let rss_xml = rss::generate_rss(&rss_entries);
    let rss_path = output_dir.join("feed.rss");
    fs::write(&rss_path, &rss_xml).unwrap_or_else(|e| {
        eprintln!("Error writing {}: {}", rss_path.display(), e);
    });

    eprintln!("=== Done! ===");
}
