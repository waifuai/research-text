import os
import markdown
import html
import re

# Define the directories to process
DIRECTORIES_TO_PROCESS = [
    "ico-sol",
    "agent-chats",
    "books",
    "kinematics",
    "papers"
]

# Define the output HTML file
OUTPUT_FILE = "combined_research.html"

# Basic HTML structure and CSS
HTML_TEMPLATE_START = """<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Combined Research Text</title>
    <style>
        body {{ font-family: sans-serif; line-height: 1.6; margin: 20px; }}
        section {{ border: 1px solid #ccc; margin-bottom: 20px; padding: 15px; border-radius: 5px; scroll-margin-top: 10px; /* Offset for fixed header if needed */ }}
        article {{ border-top: 1px dashed #eee; margin-top: 15px; padding-top: 15px; scroll-margin-top: 10px; }}
        h1 {{ color: #333; border-bottom: 2px solid #eee; padding-bottom: 5px; }}
        h2 {{ color: #555; font-size: 1.2em; margin-top: 0; }} /* File name */
        h3, h4, h5, h6 {{ color: #666; margin-top: 1em; margin-bottom: 0.5em; }} /* Converted MD headings */
        pre {{ background-color: #f8f8f8; border: 1px solid #ddd; padding: 10px; border-radius: 3px; white-space: pre-wrap; word-wrap: break-word; font-family: monospace; }}
        code {{ font-family: monospace; }}
        .filename {{ font-weight: bold; color: #0056b3; }}
        .directory-title {{ color: #0056b3; }}
        .toc {{ border: 1px solid #e0e0e0; background-color: #f9f9f9; padding: 10px 15px; margin-bottom: 15px; border-radius: 4px; }}
        .toc ul {{ list-style-type: none; padding-left: 0; }}
        .toc li {{ margin-bottom: 5px; }}
        .toc a {{ text-decoration: none; color: #0056b3; }}
        .toc a:hover {{ text-decoration: underline; }}
        .back-link {{ font-size: 0.9em; margin-top: 5px; }}
    </style>
</head>
<body>
<h1>Combined Research Content</h1>
"""

HTML_TEMPLATE_END = """
</body>
</html>
"""

def create_safe_id(text):
    """Creates a URL-safe ID from a string."""
    # Lowercase
    safe_id = text.lower()
    # Remove invalid chars (keep alphanumeric, hyphen, underscore)
    safe_id = re.sub(r'[^\w\-]+', '-', safe_id)
    # Remove leading/trailing hyphens
    safe_id = safe_id.strip('-')
    # Ensure it doesn't start with a digit
    if safe_id and safe_id[0].isdigit():
        safe_id = 'id-' + safe_id
    # Handle potentially empty IDs after sanitization
    if not safe_id:
        # Use a hash or a counter if needed for truly unique empty cases
        # For simplicity here, just use a default or raise error
        safe_id = 'invalid-id-' + str(hash(text)) # Basic fallback
    return safe_id

def convert_markdown_headings_to_html(md_text):
    """
    Converts only Markdown headings (#, ##, etc.) to HTML h tags (starting from h3).
    Escapes the rest of the content to display as plain text within paragraphs.
    """
    lines = md_text.splitlines()
    html_lines = []
    in_paragraph = False

    for line in lines:
        stripped_line = line.strip()
        heading_match = re.match(r'^(#+)\s+(.*)', stripped_line)

        if heading_match:
            if in_paragraph:
                html_lines.append("</p>")
                in_paragraph = False
            level = len(heading_match.group(1))
            level = min(level, 6) # Cap heading level at h6
            content = html.escape(heading_match.group(2).strip())
            # Start MD headings from h3 (h1=Overall, h2=Filename)
            html_level = min(level + 2, 6)
            html_lines.append(f"<h{html_level}>{content}</h{html_level}>")
        else:
            escaped_line = html.escape(line)
            if stripped_line: # Non-empty line
                if not in_paragraph:
                    html_lines.append("<p>")
                    in_paragraph = True
                html_lines.append(escaped_line + "<br>") # Use <br> for line breaks within paragraph
            elif in_paragraph: # Empty line closes paragraph
                 html_lines.append("</p>")
                 in_paragraph = False
            # else: ignore multiple blank lines outside paragraphs

    if in_paragraph: # Close any open paragraph at the end
        html_lines.append("</p>")

    return "\n".join(html_lines)


def process_directory(directory_path, section_id):
    """
    Processes files in a directory.
    Returns a tuple: (HTML content string, list of (filename, file_id) tuples for TOC).
    """
    html_content_parts = []
    file_toc_items = [] # Store tuples of (filename, file_id)

    if not os.path.isdir(directory_path):
        print(f"Warning: Directory not found: {directory_path}")
        return "", [] # Return empty string and empty list

    section_toc_id = f"toc-{section_id}"
    section_title = directory_path.replace("-", " ").title()

    # Sort files for consistent order
    try:
        # Attempt natural sorting (e.g., 1, 2, 10 instead of 1, 10, 2)
        files_list = sorted(os.listdir(directory_path), key=lambda x: [int(c) if c.isdigit() else c for c in re.split('([0-9]+)', x)])
    except:
        # Fallback to simple alphabetical sort if natural sort fails
        files_list = sorted(os.listdir(directory_path))


    for filename in files_list:
        file_path = os.path.join(directory_path, filename)
        if os.path.isfile(file_path):
            print(f"Processing: {file_path}")
            file_id = create_safe_id(f"{directory_path}-{filename}")
            file_toc_items.append((filename, file_id)) # Add to TOC list

            html_content_parts.append(f'<article id="{file_id}">') # Add ID to article
            html_content_parts.append(f'<h2><span class="filename">{html.escape(filename)}</span></h2>')
            # Add back link
            html_content_parts.append(f'<p class="back-link"><a href="#{section_toc_id}">Back to {html.escape(section_title)} TOC</a></p>')

            content = ""
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    content = f.read()
            except UnicodeDecodeError:
                try:
                    with open(file_path, 'r', encoding='latin-1') as f:
                        content = f.read()
                    print(f"  - Read {filename} with latin-1 encoding.")
                except Exception as e_read:
                    print(f"  - Error reading {filename}: {e_read}")
                    content = f"[Error reading file: {html.escape(str(e_read))}]"
            except Exception as e_read:
                 print(f"  - Error reading {filename}: {e_read}")
                 content = f"[Error reading file: {html.escape(str(e_read))}]"


            _, ext = os.path.splitext(filename)
            ext = ext.lower()

            if ext == '.md':
                processed_content = convert_markdown_headings_to_html(content)
                html_content_parts.append(f'<div class="markdown-content">{processed_content}</div>')
            elif ext == '.py':
                 escaped_content = html.escape(content)
                 html_content_parts.append(f"<pre><code>{escaped_content}</code></pre>")
            elif ext == '.txt':
                 escaped_content = html.escape(content)
                 html_content_parts.append(f"<pre>{escaped_content}</pre>")
            else:
                 # Default for other file types: treat as plain text
                 escaped_content = html.escape(content)
                 html_content_parts.append(f"<pre>{escaped_content}</pre>")

            html_content_parts.append("</article>")

    return "\n".join(html_content_parts), file_toc_items

def main():
    all_html_content = [HTML_TEMPLATE_START]
    main_toc_html = ['<nav class="toc"><h2>Table of Contents</h2><ul>']

    print("Starting HTML combination process...")

    # --- Main TOC Generation ---
    for directory in DIRECTORIES_TO_PROCESS:
        if os.path.isdir(directory):
            section_id = create_safe_id(directory)
            section_title = directory.replace("-", " ").title()
            main_toc_html.append(f'<li><a href="#{section_id}">{html.escape(section_title)}</a></li>')
    main_toc_html.append('</ul></nav>')
    all_html_content.append("\n".join(main_toc_html)) # Add Main TOC near the top

    # --- Process Directories and Generate Content ---
    for directory in DIRECTORIES_TO_PROCESS:
        print(f"\nProcessing directory: {directory}")
        section_id = create_safe_id(directory)
        section_title = directory.replace("-", " ").title()

        all_html_content.append(f'<section id="{section_id}">') # Add section ID
        all_html_content.append(f'<h1 class="directory-title">{html.escape(section_title)}</h1>')

        # Process directory and get content + file TOC items
        dir_html, file_toc_items = process_directory(directory, section_id) # Pass section_id

        # Generate and add section mini-TOC
        if file_toc_items:
            section_toc_id = f"toc-{section_id}" # ID for the mini-TOC itself
            mini_toc_html = [f'<nav class="toc" id="{section_toc_id}"><h3>Files in {html.escape(section_title)}</h3><ul>']
            for fname, f_id in file_toc_items:
                mini_toc_html.append(f'<li><a href="#{f_id}">{html.escape(fname)}</a></li>')
            mini_toc_html.append('</ul></nav>')
            all_html_content.append("\n".join(mini_toc_html)) # Add Mini-TOC

        all_html_content.append(dir_html) # Add the processed file content
        all_html_content.append("</section>")

    all_html_content.append(HTML_TEMPLATE_END)

    final_html = "\n".join(all_html_content)

    print(f"\nWriting combined HTML to {OUTPUT_FILE}...")
    try:
        with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
            f.write(final_html)
        print("Successfully created combined_research.html")
    except Exception as e:
        print(f"Error writing HTML file: {e}")

if __name__ == "__main__":
    main()