#!/usr/bin/env python3
"""
Convert research-text consolidated-research/ to HTML-only docs/ for GitHub Pages.
- Deletes all .md and .json from docs/
- Converts all .md to .html using template from 3-html.txt
- Builds section index pages + root landing page
"""

import os
import re
import shutil
import json
import markdown

REPO_ROOT = os.path.dirname(os.path.abspath(__file__))
SOURCE_DIR = os.path.join(REPO_ROOT, "consolidated-research")
DOCS_DIR = os.path.join(REPO_ROOT, "docs")

HTML_TEMPLATE = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{title}</title>
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css">
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js"></script>
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex/contrib/auto-render.min.js"
    onload="renderMathInElement(document.body, {{delimiters: [{{left: '$$', right: '$$', display: true}}, {{left: '$', right: '$', display: false}}]}});"></script>
<style>
  body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; max-width: 900px; margin: 0 auto; padding: 2rem; line-height: 1.6; color: #e6edf3; background: #0d1117; }}
  pre {{ background: #161b22; padding: 1rem; border-radius: 6px; overflow-x: auto; border: 1px solid #30363d; }}
  code {{ background: #161b22; padding: 0.2em 0.4em; border-radius: 3px; font-size: 0.9em; }}
  pre code {{ background: none; padding: 0; }}
  table {{ border-collapse: collapse; width: 100%; margin: 1rem 0; }}
  th, td {{ border: 1px solid #30363d; padding: 8px; text-align: left; }}
  th {{ background: #161b22; }}
  a {{ color: #58a6ff; text-decoration: none; }}
  a:hover {{ text-decoration: underline; }}
  h1, h2, h3 {{ border-bottom: 1px solid #30363d; padding-bottom: 0.3em; }}
  blockquote {{ border-left: 4px solid #30363d; padding-left: 1rem; color: #8b949e; }}
  .back-link {{ display: inline-block; margin-bottom: 1rem; color: #8b949e; font-size: 0.9rem; }}
</style>
</head>
<body>
<a class="back-link" href="{back_href}">&larr; Back to index</a>
{body}
</body>
</html>"""

LANDING_CSS = """<style>
:root {
  --bg: #0d1117;
  --card-bg: #161b22;
  --border: #30363d;
  --text: #e6edf3;
  --text-muted: #8b949e;
  --accent: #58a6ff;
  --accent2: #7ee787;
  --accent3: #d2a8ff;
}
* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: var(--bg); color: var(--text); line-height: 1.6; padding: 2rem; }
.container { max-width: 1100px; margin: 0 auto; }
header { text-align: center; margin-bottom: 3rem; padding: 2rem 0; border-bottom: 1px solid var(--border); }
header h1 { font-size: 2.5rem; font-weight: 700; background: linear-gradient(135deg, var(--accent), var(--accent3)); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text; margin-bottom: 0.5rem; }
header p { color: var(--text-muted); font-size: 1.1rem; }
section { margin-bottom: 2.5rem; }
h2 { font-size: 1.4rem; margin-bottom: 0.75rem; padding-bottom: 0.4rem; border-bottom: 1px solid var(--border); display: flex; align-items: baseline; gap: 0.5rem; }
.count { font-size: 0.85rem; color: var(--text-muted); font-weight: 400; }
.section-desc { color: var(--text-muted); font-size: 0.9rem; margin-bottom: 1rem; }
.link-list { display: flex; flex-direction: column; gap: 0.25rem; }
.link-list a { color: var(--accent); text-decoration: none; padding: 0.4rem 0.75rem; border-radius: 6px; transition: background 0.15s; font-size: 0.95rem; }
.link-list a:hover { background: var(--card-bg); text-decoration: underline; }
footer { text-align: center; color: var(--text-muted); font-size: 0.8rem; padding-top: 2rem; border-top: 1px solid var(--border); margin-top: 2rem; }
</style>"""


def extract_title(md_path):
    """Extract title from markdown file."""
    try:
        with open(md_path, 'r', encoding='utf-8', errors='replace') as f:
            content = f.read()
    except:
        return os.path.splitext(os.path.basename(md_path))[0]

    # Try first # heading
    m = re.search(r'^#{1,3}\s+(.+)', content, re.MULTILINE)
    if m:
        return clean_title(m.group(1))

    # Try filename
    name = os.path.splitext(os.path.basename(md_path))[0]
    return clean_title(name)


def clean_title(title):
    """Strip leading number patterns and clean up."""
    title = re.sub(r'^#+\s*', '', title)
    title = re.sub(r'^(\d+[\.:]?\d*\.?\s*)+', '', title)
    title = re.sub(r'^[\.\:\s]+', '', title)
    title = title.strip()
    if not title:
        title = "Untitled"
    return title


def fix_md_links(html):
    """Convert .md links to .html in converted HTML."""
    return re.sub(r'href="([^"]*?)\.md"', r'href="\1.html"', html)


def has_math(content):
    """Check if markdown has KaTeX math notation."""
    return bool(re.search(r'\$[^$\n]+\$|^\$\$', content, re.MULTILINE))


def convert_md_to_html(md_path, output_path, back_href="index.html"):
    """Convert a single .md file to HTML."""
    with open(md_path, 'r', encoding='utf-8', errors='replace') as f:
        md_content = f.read()

    title = extract_title(md_path)
    use_math = has_math(md_content)

    md_ext = markdown.Markdown(extensions=['tables', 'fenced_code'])
    html_body = md_ext.convert(md_content)
    html_body = fix_md_links(html_body)

    # Use template
    html = HTML_TEMPLATE.format(title=title, body=html_body, back_href=back_href)

    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(html)

    return title


def build_section_index(section_dir, section_name, titles_map):
    """Build index.html for a section directory."""
    links_html = ""
    for href, title in sorted(titles_map.items(), key=lambda x: x[0]):
        if href == "index.html":
            continue
        links_html += f'    <a href="{href}">{title}</a>\n'

    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{section_name}</title>
{LANDING_CSS}
</head>
<body>
<div class="container">
<header>
<h1>{section_name}</h1>
<p>Research documentation</p>
</header>
<section>
<h2>Documents <span class="count">({len(titles_map) - 1})</span></h2>
<div class="link-list">
{links_html}</div>
</section>
<footer>
<p><a href="../index.html">&larr; Back to main index</a></p>
</footer>
</div>
</body>
</html>"""

    with open(os.path.join(section_dir, "index.html"), 'w', encoding='utf-8') as f:
        f.write(html)


def build_root_index(sections):
    """Build root docs/index.html landing page."""
    sections_html = ""
    total = 0
    for section_name, section_path, count in sections:
        total += count
        sections_html += f"""
<section>
<h2>{section_name} <span class="count">({count})</span></h2>
<p class="section-desc">Research papers and documentation</p>
<div class="link-list">
    <a href="{section_path}/index.html">Browse {section_name} &rarr;</a>
</div>
</section>"""

    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Research Documentation</title>
{LANDING_CSS}
</head>
<body>
<div class="container">
<header>
<h1>Research Documentation</h1>
<p>Comprehensive research on tokenomics, quantum computing, kinematics, and more</p>
</header>
{sections_html}
<footer>
<p>Total: {total} documents across {len(sections)} sections</p>
<p><a href="https://github.com/waifuai/research-text">Source on GitHub</a></p>
</footer>
</div>
</body>
</html>"""

    with open(os.path.join(DOCS_DIR, "index.html"), 'w', encoding='utf-8') as f:
        f.write(html)


def main():
    # Step 1: Delete all .md and .json from docs/
    deleted = 0
    for root, dirs, files in os.walk(DOCS_DIR):
        for f in files:
            if f.endswith('.md') or f.endswith('.json'):
                os.remove(os.path.join(root, f))
                deleted += 1
    print(f"Deleted {deleted} .md/.json files from docs/")

    # Step 2: Convert each section
    sections_info = []
    section_dirs = sorted(os.listdir(SOURCE_DIR))

    for section_name in section_dirs:
        section_source = os.path.join(SOURCE_DIR, section_name)
        section_docs = os.path.join(DOCS_DIR, "consolidated-research", section_name)

        if not os.path.isdir(section_source):
            continue
        if section_name == '.venv' or section_name == '__pycache__':
            continue

        # Find all .md files in this section (non-recursive, skip lessons/ etc)
        md_files = []
        for f in sorted(os.listdir(section_source)):
            if f.endswith('.md') and os.path.isfile(os.path.join(section_source, f)):
                md_files.append(f)

        if not md_files:
            continue

        print(f"\n=== {section_name}: {len(md_files)} .md files ===")

        titles_map = {}
        converted = 0

        for md_file in md_files:
            md_path = os.path.join(section_source, md_file)
            html_name = os.path.splitext(md_file)[0] + '.html'
            html_path = os.path.join(section_docs, html_name)

            # README.md -> index.html
            if md_file == 'README.md':
                html_path = os.path.join(section_docs, 'index.html')
                html_name = 'index.html'

            title = convert_md_to_html(md_path, html_path, back_href="index.html")
            titles_map[html_name] = title
            converted += 1

        print(f"  Converted {converted} files")

        # Build section index
        if 'index.html' not in titles_map:
            build_section_index(section_docs, section_name, titles_map)
            print(f"  Built section index.html")

        sections_info.append((section_name, section_name, converted))

    # Step 3: Build root index
    build_root_index(sections_info)
    print(f"\nBuilt root docs/index.html")

    # Step 4: Verify
    html_count = 0
    md_count = 0
    json_count = 0
    for root, dirs, files in os.walk(DOCS_DIR):
        for f in files:
            if f.endswith('.html'):
                html_count += 1
            elif f.endswith('.md'):
                md_count += 1
            elif f.endswith('.json'):
                json_count += 1

    print(f"\n=== VERIFICATION ===")
    print(f"HTML files: {html_count}")
    print(f"MD files remaining: {md_count}")
    print(f"JSON files remaining: {json_count}")
    print("DONE!")


if __name__ == "__main__":
    main()
