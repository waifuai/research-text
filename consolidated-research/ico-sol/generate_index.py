#!/usr/bin/env python3
import os
import json
import re

def natural_sort_key(filename):
    # Extract numbers and sort naturally
    return [int(text) if text.isdigit() else text.lower() for text in re.split(r'(\d+)', filename)]

def main():
    # Get all .json files except 00-index.json if it exists
    json_files = [f for f in os.listdir('.') if f.endswith('.json') and not f.startswith('00-index')]
    json_files.sort(key=natural_sort_key)

    data_list = []
    for json_file in json_files:
        try:
            with open(json_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
            data['json_file'] = json_file
            data_list.append(data)
        except Exception as e:
            print(f"Error loading {json_file}: {e}")
            continue

    index_lines = []

    # TOC
    index_lines.append('<a name="top"></a>')
    index_lines.append("# Index of Papers")
    index_lines.append("")
    for i, data in enumerate(data_list):
        title = data.get('title', 'No Title')
        file_prefix = data['json_file'].replace('.json', '')
        idx = f"{i+1:02d}"
        index_lines.append(f'<a id="toc-{idx}"></a>')
        index_lines.append(f"- [{file_prefix} {title}](#section-{idx})")
    index_lines.append("")

    # Sections
    for i, data in enumerate(data_list):
        title = data.get('title', 'No Title')
        link = data.get('link', '')
        description = data.get('description', 'No Description')
        idx = f"{i+1:02d}"


        index_lines.append(f'<a id="section-{idx}"></a>')
        index_lines.append(f"# {title}")
        index_lines.append("")
        if link:
            link_md = link.lstrip('./')
            index_lines.append(f"Full Link: [{link_md}]({link})")
            index_lines.append("")
        index_lines.append(description)
        index_lines.append("")
        index_lines.append(f"[Back to Top](#toc-{idx})")
        index_lines.append("")


    # Write to 00-index.md
    with open('00-index.md', 'w', encoding='utf-8') as f:
        f.writelines('\n'.join(index_lines))

    print("00-index.md generated successfully.")

if __name__ == '__main__':
    main()