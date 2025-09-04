import re

with open('06-qc_agi.md', 'r', encoding='utf-8') as f:
    lines = f.readlines()

chapter_start = []
for i, line in enumerate(lines):
    if re.match(r'^## Chapter \d+', line):
        chapter_start.append(i)

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
    # Find the first ###
    sub_0_end = len(ch_lines)
    for l in range(1, len(ch_lines)):
        if re.match(r'^### \d+\.\d+', ch_lines[l]):
            sub_0_end = l
            break
    if sub_0_end > 1:
        content = ''.join(ch_lines[1:sub_0_end])
        with open(f'chapter_{ch_num}_subsection_0.md', 'w', encoding='utf-8') as out:
            out.write(content.strip())
    # Now for subsections
    last_end = sub_0_end
    sub_y = 1
    k = sub_0_end
    while k < len(ch_lines):
        if re.match(r'^### \d+\.\d+', ch_lines[k]):
            # Write the previous section
            sub_content = ch_lines[last_end:k]
            if sub_content:
                # Adjust headers
                if re.match(r'^### \d+\.\d+', sub_content[0]):
                    sub_content[0] = '#' + sub_content[0][3:]
                for i in range(1, len(sub_content)):
                    if sub_content[i].startswith('##') and not sub_content[i].startswith('###'):
                        sub_content[i] = '#' + sub_content[i][2:]
                content = ''.join(sub_content).strip()
                with open(f'chapter_{ch_num}_subsection_{sub_y}.md', 'w', encoding='utf-8') as out:
                    out.write(content)
                sub_y +=1
            last_end = k
        k +=1
    # For the last subsection
    if last_end < len(ch_lines):
        sub_content = ch_lines[last_end:]
        if sub_content and re.match(r'^### \d+\.\d+', sub_content[0]):
            # Adjust
            sub_content[0] = '#' + sub_content[0][3:]
            for i in range(1, len(sub_content)):
                if sub_content[i].startswith('##') and not sub_content[i].startswith('###'):
                    sub_content[i] = '#' + sub_content[i][2:]
            content = ''.join(sub_content).strip()
            with open(f'chapter_{ch_num}_subsection_{sub_y}.md', 'w', encoding='utf-8') as out:
                out.write(content)