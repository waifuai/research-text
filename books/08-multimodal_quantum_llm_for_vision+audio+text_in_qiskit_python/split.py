#!/usr/bin/env python3

import os
import re

def split_md_file(file_path, output_dir):
    with open(file_path, 'r') as f:
        content = f.read()

    lines = content.split('\n')
    sections = []
    current_section = {}
    # pattern for headers
    header_pattern = re.compile(r'^(#{1,3}) (.+)$')

    for i, line in enumerate(lines):
        match = header_pattern.match(line)
        if match:
            # new section
            if current_section:
                if 'content' not in current_section:
                    current_section['content'] = ''
                current_section['end_line'] = i
                sections.append(current_section)

            level = len(match.group(1))
            title = match.group(2).strip()
            current_section = {
                'level': level,
                'title': title,
                'start_line': i,
                'content': '',
                'lines': [line]
            }
            continue
        else:
            if current_section:
                current_section['lines'].append(line)
                current_section['content'] += line + '\n'

    if current_section:
        sections.append(current_section)

    # now identify chapters and subsections
    chapters = []
    subsections = []
    for section in sections:
        if section['level'] == 1:  # #
            chapters.append(section)
        elif section['level'] == 2:  # ##
            # Check if it's a chapter or subsection
            if 'Introduction' in section['title'] or section['title'] == 'Understanding Vision, Audio, and Text Data' or section['title'] == 'Building a Multimodal Quantum Neural Network' or section['title'] == 'Developing the Quantum LLMs' or section['title'] == 'Multimodal Vision-Audio-Text Tasks' or section['title'] == 'Challenges and Future Directions' or section['title'] == 'Appendix':
                chapters.append(section)
            else:
                subsections.append(section)
        elif section['level'] == 3:  # ###
            subsections.append(section)

    # to assign chapter numbers
    chapter_titles = [
        'Introduction to Quantum Machine Learning with Qiskit',
        'Understanding Vision, Audio, and Text Data',
        'Building a Multimodal Quantum Neural Network',
        'Developing the Quantum LLMs',
        'Multimodal Vision-Audio-Text Tasks',
        'Challenges and Future Directions',
        'Appendix'
    ]

    chapter_num = 1
    subsection_num = {}

    for sec in sections:
        if sec['level'] in [2, 3]:
            title = sec['title']
            if title in chapter_titles:
                chap_num = chapter_titles.index(title) + 1
                chapter_num = chap_num
                subsection_num[chap_num] = 0

            # create file
            filename = f"chapter_{chapter_num}_subsection_{subsection_num.get(chapter_num, 0)}.md"
            filepath = os.path.join(output_dir, filename)

            # adjust headers based on level
            content = '\n'.join(sec['lines'])
            if sec['level'] == 2:
                # demote ## to #
                content = re.sub(r'^##', '#', content, flags=re.MULTILINE)
            elif sec['level'] == 3:
                # demote ### to ##
                content = re.sub(r'^###', '##', content, flags=re.MULTILINE)
            # adjust any deeper
            content = re.sub(r'^####', '###', content, flags=re.MULTILINE)
            content = re.sub(r'^#####', '####', content, flags=re.MULTILINE)

            with open(filepath, 'w') as f:
                f.write(content)

            subsection_num[chapter_num] = subsection_num.get(chapter_num, 0) + 1

    print(f"Split into {chapter_num} chapters with multiple subsections.")

    return sections

if __name__ == '__main__':
    file_path = '08-multimodal_quantum_llm_for_vision+audio+text_in_qiskit_python.md'
    output_dir = '.'
    split_md_file(file_path, output_dir)