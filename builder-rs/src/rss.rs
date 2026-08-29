use crate::ast::*;
use crate::render::{heading_to_slug, heading_to_rss_title};

const WEBSITE_URL: &str = "https://bollu.github.io";
const RSS_DESCRIPTION: &str = "A universe of Sorts";
const ARTICLES_URL_PREFIX: &str = "/articles/";

/// An article entry for RSS: title text and URL slug.
pub struct RssEntry {
    pub title: String,
    pub slug: String,
}

/// Extract RSS entries from the block list (one per H1 heading).
pub fn extract_rss_entries(blocks: &[Block], input: &str) -> Vec<RssEntry> {
    blocks.iter().filter_map(|block| {
        if let Block::Heading { level: 1, content, .. } = block {
            let title = heading_to_rss_title(content, input);
            let slug = heading_to_slug(content, input);
            Some(RssEntry { title, slug })
        } else {
            None
        }
    }).collect()
}

/// Generate the RSS feed XML string from a list of entries.
pub fn generate_rss(entries: &[RssEntry]) -> String {
    let mut out = String::new();
    out.push_str("<?xml version=\"1.0\"?>\n");
    out.push_str("<rss version=\"2.0\">\n");
    out.push_str("<channel>\n");
    out.push_str("<title>A universe of sorts</title>\n");
    out.push_str("<link>https://bollu.github.io/</link>\n");
    out.push_str(&format!("<description>{}</description>\n", RSS_DESCRIPTION));

    for entry in entries {
        out.push_str("  <item>\n");
        out.push_str(&format!("    <title>{}</title>\n", entry.title));
        out.push_str(&format!(
            "    <guid>{}{}{}.html</guid>\n",
            WEBSITE_URL, ARTICLES_URL_PREFIX, entry.slug
        ));
        out.push_str(&format!(
            "    <link>{}{}{}.html</link>\n",
            WEBSITE_URL, ARTICLES_URL_PREFIX, entry.slug
        ));
        out.push_str("  </item>\n");
    }

    out.push_str("</channel>\n");
    out.push_str("</rss>");
    out
}
