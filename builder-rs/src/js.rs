use quick_js::{Context, JsValue};
use crate::render::{CodeHighlighter, LatexCompiler};
use std::cell::RefCell;

/// A KaTeX compiler backed by QuickJS (embedded JS engine, like Duktape in the C++ version).
pub struct QuickJsKatex {
    ctx: RefCell<Context>,
}

impl QuickJsKatex {
    pub fn new(katex_js_path: &str) -> Result<Self, String> {
        let ctx = Context::new()
            .map_err(|e| format!("failed to create QuickJS context: {:?}", e))?;

        let source = std::fs::read_to_string(katex_js_path)
            .map_err(|e| format!("failed to read {}: {}", katex_js_path, e))?;

        ctx.eval(&source)
            .map_err(|e| format!("failed to evaluate katex.min.js: {:?}", e))?;

        Ok(Self { ctx: RefCell::new(ctx) })
    }

    fn render_impl(&self, source: &str, display_mode: bool) -> Result<String, String> {
        let ctx = self.ctx.borrow();
        let dm = if display_mode { "true" } else { "false" };
        // Escape for JS string literal
        let escaped = js_string_escape(source);
        let js = format!(
            "katex.renderToString('{}', {{ displayMode: {} }})",
            escaped, dm
        );
        match ctx.eval(&js) {
            Ok(JsValue::String(s)) => Ok(s),
            Ok(other) => Ok(format!("{:?}", other)),
            Err(e) => Err(format!("katex error: {:?}", e)),
        }
    }
}

// QuickJS Context uses RefCell, not thread-safe. We use unsafe Send+Sync
// since each article will get its own thread-local context in practice.
unsafe impl Send for QuickJsKatex {}
unsafe impl Sync for QuickJsKatex {}

impl LatexCompiler for QuickJsKatex {
    fn render(&self, source: &str, display_mode: bool) -> Result<String, String> {
        self.render_impl(source, display_mode)
    }
}

/// A Prism.js highlighter backed by QuickJS.
pub struct QuickJsPrism {
    ctx: RefCell<Context>,
}

impl QuickJsPrism {
    pub fn new(prism_js_path: &str) -> Result<Self, String> {
        let ctx = Context::new()
            .map_err(|e| format!("failed to create QuickJS context: {:?}", e))?;

        let source = std::fs::read_to_string(prism_js_path)
            .map_err(|e| format!("failed to read {}: {}", prism_js_path, e))?;

        ctx.eval(&source)
            .map_err(|e| format!("failed to evaluate prism.js: {:?}", e))?;

        Ok(Self { ctx: RefCell::new(ctx) })
    }

    fn highlight_impl(&self, code: &str, lang: &str) -> Result<String, String> {
        if lang.is_empty() || lang == "text" {
            return Ok(crate::render::html_escape(code));
        }

        let ctx = self.ctx.borrow();
        let escaped_code = js_string_escape(code);
        let js = format!(
            "Prism.highlight('{}', Prism.languages.{}, '{}')",
            escaped_code, lang, lang
        );
        match ctx.eval(&js) {
            Ok(JsValue::String(s)) => Ok(s),
            Ok(other) => Ok(format!("{:?}", other)),
            Err(e) => Err(format!("prism highlight error for lang '{}': {:?}", lang, e)),
        }
    }
}

unsafe impl Send for QuickJsPrism {}
unsafe impl Sync for QuickJsPrism {}

impl CodeHighlighter for QuickJsPrism {
    fn highlight(&self, code: &str, lang: &str) -> Result<String, String> {
        self.highlight_impl(code, lang)
    }
}

/// Escape a string for use inside a JavaScript single-quoted string literal.
fn js_string_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 16);
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '\'' => out.push_str("\\'"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(c),
        }
    }
    out
}
