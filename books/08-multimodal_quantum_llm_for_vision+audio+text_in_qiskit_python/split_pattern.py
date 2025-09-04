#!/usr/bin/env/python3

import re

with open('08-multimodal_quantum_llm_for_vision+audio+text_in_qiskit_python.md', 'r', encoding='utf-8') as f:
    lines = f.readlines()

# Define sections with patterns for chapters and subsections
sections = [
    {'pattern': r'## Introduction to Quantum Machine Learning with Qiskit', 'file': 'chapter_1_subsection_0.md'},
    {'pattern': r'### What is Quantum Machine Learning\?', 'file': 'chapter_1_subsection_1.md'},
    {'pattern': r'### Why Use Qiskit for Quantum Machine Learning\?', 'file': 'chapter_1_subsection_2.md'},
    {'pattern': r'### Introduction to Quantum Computing Fundamentals', 'file': 'chapter_1_subsection_3.md'},
    {'pattern': r'### Qiskit Overview and Installation', 'file': 'chapter_1_subsection_4.md'},
    {'pattern': r'### Basic Quantum Circuit Building with Qiskit', 'file': 'chapter_1_subsection_5.md'},
    {'pattern': r'### Introduction to Supervised Learning', 'file': 'chapter_1_subsection_6.md'},
    {'pattern': r'### Introduction to Multimodal Data Fusion', 'file': 'chapter_1_subsection_7.md'},
    {'pattern': r'### Motivation for Vision-Audio-Text Fusion', 'file': 'chapter_1_subsection_8.md'},
    {'pattern': r'## Understanding Vision, Audio, and Text Data', 'file': 'chapter_2_subsection_0.md'},
    {'pattern': r'### Image Representation and Feature Extraction', 'file': 'chapter_2_subsection_1.md'},
    {'pattern': r'### Audio Signal Processing Techniques', 'file': 'chapter_2_subsection_2.md'},
    {'pattern': r'### Natural Language Processing Techniques', 'file': 'chapter_2_subsection_3.md'},
    {'pattern': r'### Data Preprocessing and Cleaning', 'file': 'chapter_2_subsection_4.md'},
    {'pattern': r'### Feature Engineering for Vision, Audio, and Text', 'file': 'chapter_2_subsection_5.md'},
    {'pattern': r'### Common Data Formats for Vision, Audio, and Text', 'file': 'chapter_2_subsection_6.md'},
    {'pattern': r'## Building a Multimodal Quantum Neural Network', 'file': 'chapter_3_subsection_0.md'},
    {'pattern': r'### Quantum Feature Encoding for Vision, Audio, and Text', 'file': 'chapter_3_subsection_1.md'},
    {'pattern': r'### Designing a Quantum Architecture for Multimodal Fusion', 'file': 'chapter_3_subsection_2.md'},
    {'pattern': r'### Implementing a Quantum Layer for each modality', 'file': 'chapter_3_subsection_3.md'},
    {'pattern': r'### Creating Entanglement for Multimodal Data Fusion', 'file': 'chapter_3_subsection_4.md'},
    {'pattern': r'### Hybrid Quantum-Classical Approach for Vision-Audio-Text', 'file': 'chapter_3_subsection_5.md'},
    {'pattern': r'### Quantum Circuit Design considerations for scalability', 'file': 'chapter_3_subsection_6.md'},
    {'pattern': r'## Developing the Quantum LLMs', 'file': 'chapter_4_subsection_0.md'},
    {'pattern': r'### Designing the Quantum Language Model Architecture', 'file': 'chapter_4_subsection_1.md'},
    {'pattern': r'### Integrating Quantum Layers into the Multimodal Network', 'file': 'chapter_4_subsection_2.md'},
    {'pattern': r'### Training the Multimodal Quantum Language Model with Qiskit', 'file': 'chapter_4_subsection_3.md'},
    {'pattern': r'### Understanding Quantum Training Dynamics', 'file': 'chapter_4_subsection_4.md'},
    {'pattern': r'### Quantum Optimizer Selection', 'file': 'chapter_4_subsection_5.md'},
    {'pattern': r'### Quantum Gradient Estimation Techniques', 'file': 'chapter_4_subsection_6.md'},
    {'pattern': r'### Evaluating Quantum Model Performance', 'file': 'chapter_4_subsection_7.md'},
    {'pattern': r'### Strategies to address Quantum Noise and Errors', 'file': 'chapter_4_subsection_8.md'},
    {'pattern': r'### Strategies for managing Qiskit execution resources', 'file': 'chapter_4_subsection_9.md'},
    {'pattern': r'### Hybrid Quantum-Classical Approach for Vision-Audio-Text', 'file': 'chapter_4_subsection_10.md'},  # note duplicate title, hy handle
    {'pattern': r'### Quantum Circuit Design considerations for scalability', 'file': 'chapter_4_subsection_11.md'},  # duplicate, skip
    {'pattern': r'## Multimodal Vision-Audio-Text Tasks', 'file': 'chapter_5_subsection_0.md'},
    {'pattern': r'### Image Captioning with Vision-Audio-Text Data', 'file': 'chapter_5_subsection_1.md'},
    {'pattern': r'### Audio-Visual Event Recognition with Quantum LLMs', 'file': 'chapter_5_subsection_2.md'},
    {'pattern': r'### Cross-Modal Similarity Search', 'file': 'chapter_5_subsection_3.md'},
    {'pattern': r'### Sentiment Analysis on Multimodal Data', 'file': 'chapter_5_subsection_4.md'},
    {'pattern': r'### Question Answering across Vision-Audio-Text Data', 'file': 'chapter_5_subsection_5.md'},
    {'pattern': r'### Case Study: Multimodal Image Classification', 'file': 'chapter_5_subsection_6.md'},
    {'pattern': r'## Challenges and Future Directions', 'file': 'chapter_6_sub_section_0.md'},  # note underscore
    {'pattern': r'### Limitations of Current Quantum Hardware', 'file': 'chapter_6_subsection_1.md'},
    {'pattern': r'### Overcoming Noise and Error in Quantum LLMs', 'file': 'chapter_6_subsection_2.md'},
    {'pattern': r'### Quantum Algorithm Design Considerations', 'file': 'chapter_6_subsection_3.md'},
    {'pattern': r'### Scalability of Quantum LLMs', 'file': 'chapter_6_subsection_4.md'},
    {'pattern': r'### Future Research Directions for Multimodal Quantum LLMs', 'file': 'chapter_6_subsection_5.md'},
    {'pattern': r'### Exploring Quantum Embeddings for Multimodal Data', 'file': 'chapter_7_subsection_6.md'},
    {'pattern': r'### Integration with.Classical NLP and Computer Vision Libraries', 'file': 'chapter_7_subsection_7.md'},
    {'pattern': r'## Appendix', 'file': 'chapter_7_subsection_0.md'},
    {'pattern': r'### Qiskit Libraries and Functions Reference', 'file': 'chapter_7_subsection_1.md'},
    {'pattern': r'### List of Useful Datasets', 'file': 'chapter_7_subsection_2.md'},
    {'pattern': r'### Experimental Data and Results', 'file': 'chapter_7_subsection_3.md'},
]

def find_start_line(pattern):
    for i in range(len(lines)):
        if re.search(pattern, lines[i]):
            return i
    return None

for i, section in enumerate(sections):
    start = find_start_line(section['pattern'])
    if start is None:
        print(f"Pattern not found: {section['pattern']}")
        continue
    if i < len(sections) - 1:
        next_start = find_start_line(sections[i+1]['pattern'])
        if next_start is None:
            end = len(lines)
        else:
            end = next_start
    else:
        end = len(lines)
    section_content = ''.join(lines[start:end])
    num_lines = len(section_content.split('\n'))
    print(f"Section: {section['file']}, start: {start}, end: {end}, len(content): {num_lines}")

    # Adjust headers for files
    if section['file'].endswith('_0.md'):  # Chapter files
        # Demote ## to #
        section_content = re.sub(r'^##', '#', section_content, flags=re.MULTILINE)
    else:  # Subsection files
        # Demote ### to ##
        section_content = re.sub(r'^###', '##', section_content, flags=re.MULTILINE)
    # Adjust deeper headers if any
    ### section_content = re.sub(r'^####', '###', section_content, flags=re.MULTILINE)
    section_content = re.sub(r'^#####', '####', section_content, flags=re.MULTILINE)

    with open(section['file'], 'w', encoding='utf-8') as f:
        f.write(section_content.strip())
    num_lines = len(section_content.split('\n'))
    print(f"Created {section['file']} with {num_lines} lines")

print("Splitting completed.")