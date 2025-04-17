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
        section {{ border: 1px solid #ccc; margin-bottom: 20px; padding: 15px; border-radius: 5px; }}
        article {{ border-top: 1px dashed #eee; margin-top: 15px; padding-top: 15px; }}
        h1 {{ color: #333; border-bottom: 2px solid #eee; padding-bottom: 5px; }}
        h2 {{ color: #555; font-size: 1.2em; margin-top: 0; }} /* File name */
        h3, h4, h5, h6 {{ color: #666; margin-top: 1em; margin-bottom: 0.5em; }} /* Converted MD headings */
        pre {{ background-color: #f8f8f8; border: 1px solid #ddd; padding: 10px; border-radius: 3px; white-space: pre-wrap; word-wrap: break-word; font-family: monospace; }}
        code {{ font-family: monospace; }}
        .filename {{ font-weight: bold; color: #0056b3; }}
        .directory-title {{ color: #0056b3; }}
    </style>
</head>
<body>
<h1>Combined Research Content</h1>
"""

HTML_TEMPLATE_END = """
</body>
</html>
"""

def convert_markdown_headings_to_html(md_text):
    """
    Converts only Markdown headings (#, ##, etc.) to HTML h tags.
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
            html_lines.append(f"<h{level+1}>{content}</h{level+1}>") # +1 because h1 is dir name
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


def process_directory(directory_path):
    """Processes files in a directory and returns their HTML representation."""
    html_content_parts = []

    if not os.path.isdir(directory_path):
        print(f"Warning: Directory not found: {directory_path}")
        return "" # Return empty string if directory doesn't exist

    # Sort files for consistent order, perhaps numerically if possible
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
            html_content_parts.append(f"<article>")
            html_content_parts.append(f'<h2><span class="filename">{html.escape(filename)}</span></h2>')

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

    return "\n".join(html_content_parts)

def main():
    all_html_content = [HTML_TEMPLATE_START]

    print("Starting HTML combination process...")

    for directory in DIRECTORIES_TO_PROCESS:
        print(f"\nProcessing directory: {directory}")
        all_html_content.append(f'<section id="{html.escape(directory)}">')
        all_html_content.append(f'<h1 class="directory-title">{html.escape(directory.replace("-", " ").title())}</h1>') # Title case dir name
        dir_html = process_directory(directory)
        all_html_content.append(dir_html)
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