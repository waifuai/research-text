import re

with open('07-using_multimodal_llm_to_make_interactive_3d_ai_characters_in_webvr.md', 'r', encoding='utf-8') as f:
    lines = f.readlines()

chapter_start = []
for i, line in enumerate(lines):
    if re.match(r'^## Chapter \d+', line):
        chapter_start.append(i)

# Handle pre-chapter content as intro
if chapter_start:
    pre_lines = lines[:chapter_start[0]]
    if pre_lines:
        content = ''.join(pre_lines).strip()
        if content:
            with open('chapter_1_subsection_0.md', 'w', encoding='utf-8') as out:
                out.write(content)

# Now process each chapter
for j in range(len(chapter_start)):
    ch_start = chapter_start[j]
    if j +1 < len(chapter_start):
        ch_end = chapter_start[j+1]
    else:
        ch_end = len(lines)
    ch_lines = lines[ch_start:ch_end]
    match = re.search(r'^## Chapter (\d+)', ch_lines[0])
    if match:
        ch_num = int(match.group(1))
    else:
        continue
    # Now for subsections
    last_end = 0
    sub_y = 1
    k = 0
    while k < len(ch_lines):
        if re.match(r'^### ', ch_lines[k]):
            # Write the previous section
            sub_content = ch_lines[last_end:k]
            if sub_content:
                # Adjust headers
                # Adjust headers
                for i in range(0, len(sub_content)):
                    if sub_content[i].startswith('###'):
                        sub_content[i] = '#' + sub_content[i][3:]
                    elif sub_content[i].startswith('##'):
                        sub_content[i] = '#' + sub_content[i][2:]
                    elif sub_content[i].startswith('####'):
                        sub_content[i] = '##' + sub_content[i][4:]
                    elif sub_content[i].startswith('#####'):
                        sub_content[i] = '###' + sub_content[i][5:]
                    elif sub_content[i].startswith('######'):
                        sub_content[i] = '####' + sub_content[i][6:]
                content = ''.join(sub_content).strip()
                with open(f'chapter_{ch_num}_subsection_{sub_y}.md', 'w', encoding='utf-8') as out:
                    out.write(content)
                sub_y +=1
            last_end = k
        k +=1
    # For the last subsection
    if last_end < len(ch_lines):
        sub_content = ch_lines[last_end:]
        if sub_content:
            # Adjust headers
            for i in range(0, len(sub_content)):
                if sub_content[i].startswith('###'):
                    sub_content[i] = '#' + sub_content[i][3:]
                elif sub_content[i].startswith('####'):
                    sub_content[i] = '##' + sub_content[i][4:]
                # etc
            content = ''.join(sub_content).strip()
            with open(f'chapter_{ch_num}_subsection_{sub_y}.md', 'w', encoding='utf-8') as out:
                out.write(content)