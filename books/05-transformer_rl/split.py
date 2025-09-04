import re
import os

with open('05-transformer_rl.md', 'r') as f:
    lines = f.readlines()

# Find lines with ### x.y
headers = []
for i, line in enumerate(lines):
    if re.match(r'### \d+\.\d+', line):
        headers.append(i)

# Intro: line 74 to 76: index 73 to 75
intro_start_idx = 73
intro_end_idx = 75
# Create intro file
content_intro = ''.join(lines[intro_start_idx:intro_end_idx+1])
with open('chapter_1_subsection_0.md', 'w') as f:
    f.write('# Introduction to Large Multimodal Transformer Models and Reinforcement Learning\n\n' + content_intro)

chapter = 1
subsection = 1

for i in range(len(headers)):
    section_header_line = lines[headers[i]]
    match = re.search(r'### (\d+)\.(\d+) (.+)', section_header_line)
    if not match:
        continue
    ch, sub, title = int(match.group(1)), int(match.group(2)), match.group(3).strip()

    chapter = ch
    subsection = sub

    # Content start: headers[i] + 5 (as index)
    start = headers[i] + 5
    end = headers[i+1] if i+1 < len(headers) else None
    content = ''.join(lines[start:end])
    if content.strip():
        # Demote header
        file_content = f'# {title}\n\n{content}'
        filename = f'chapter_{chapter}_subsection_{subsection}.md'
        with open(filename, 'w') as f:
            f.write(file_content)