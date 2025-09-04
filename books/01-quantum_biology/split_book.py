import re

with open('01-quantum_biology.md', 'r', encoding='utf-8') as f:
    lines = f.readlines()

sections = [
    {'pattern': r'This chapter provides', 'file': 'chapter_1_subsection_0.md'},
    {'pattern': r'The Quantum Realm and Biology: An Introduction', 'file': 'chapter_1_subsection_1.md'},
    {'pattern': r'Quantum Mechanics for Biologists: A Primer', 'file': 'chapter_1_subsection_2.md'},
    {'pattern': r'Key Concepts in Quantum Biology: Superposition, Entanglement, and Coherence', 'file': 'chapter_1_subsection_3.md'},
    {'pattern': r'Quantum Properties of Biological Molecules: A Molecular View', 'file': 'chapter_1_subsection_4.md'},
    {'pattern': r'Historical Perspectives and Early Discoveries in Quantum Biology', 'file': 'chapter_1_subsection_5.md'},
    {'pattern': r'The Importance of Environmental Factors in Quantum Biological Processes', 'file': 'chapter_1_subsection_6.md'},
    {'pattern': r'Chapter 2 delves into', 'file': 'chapter_2_subsection_0.md'},
    {'pattern': r'The Photosynthetic Machinery: Structure and Function', 'file': 'chapter_2_subsection_1.md'},
    {'pattern': r'Light Harvesting Antennas: A Quantum Perspective', 'file': 'chapter_2_subsection_2.md'},
    {'pattern': r'Energy Transfer Mechanisms: Förster Resonance Energy Transfer', 'file': 'chapter_2_subsection_3.md'},
    {'pattern': r'Quantum Coherence and Superposition in Photosynthetic Light Harvesting', 'file': 'chapter_2_subsection_4.md'},
    {'pattern': r'Photosynthetic Efficiency and Quantum Yields', 'file': 'chapter_2_subsection_5.md'},
    {'pattern': r'Case Studies of Photosynthetic Quantum Phenomena in Different Organisms', 'file': 'chapter_2_subsection_6.md'},
    {'pattern': r'Chapter 3 explores the intriguing possibility', 'file': 'chapter_3_subsection_0.md'},
    {'pattern': r'The Photoreceptor Complex: Structure and Function', 'file': 'chapter_3_subsection_1.md'},
    {'pattern': r'The Role of Retinal Isomerization in Vision', 'file': 'chapter_3_subsection_2.md'},
    {'pattern': r'Quantum Mechanical Models of Light Detection in the Retina', 'file': 'chapter_3_subsection_3.md'},
    {'pattern': r'The Quantum Efficiency of Vision', 'file': 'chapter_3_subsection_4.md'},
    {'pattern': r'Investigating the Role of Quantum Coherence in Vision', 'file': 'chapter_3_subsection_5.md'},
    {'pattern': r'Evolutionary Adaptations and Quantum Sensing in Vision', 'file': 'chapter_3_subsection_6.md'},
    {'pattern': r'Chapter 4 delves into the crucial role of electron transfer processes', 'file': 'chapter_4_subsection_0.md'},
    {'pattern': r'Electron Transfer Processes in Biological Systems', 'file': 'chapter_4_subsection_1.md'},
    {'pattern': r'Quantum Tunneling and Electron Transfer Rates', 'file': 'chapter_4_subsection_2.md'},
    {'pattern': r'Protein Structure and Electron Transfer Dynamics', 'file': 'chapter_4_subsection_3.md'},
    {'pattern': r'Quantum Mechanical Modelling of Electron Transfer Reactions', 'file': 'chapter_4_subsection_4.md'},
    {'pattern': r'Redox Reactions and Quantum Phenomena in Enzymes', 'file': 'chapter_4_subsection_5.md'},
    {'pattern': r'Examples of Quantum Electron Transfer Pathways in Respiration and Metabolism', 'file': 'chapter_4_subsection_6.md'},
    {'pattern': r'Chapter 5 delves into the intricate interplay between DNA', 'file': 'chapter_5_subsection_0.md'},
    {'pattern': r'The Quantum Nature of DNA Structure and Interactions', 'file': 'chapter_5_subsection_1.md'},
    {'pattern': r'DNA Replication and Repair: Quantum Roles\?', 'file': 'chapter_5_subsection_2.md'},
    {'pattern': r'DNA Damage and Repair: Quantum Considerations', 'file': 'chapter_5_subsection_3.md'},
    {'pattern': r'Quantum Entanglement and Genetic Information Transfer', 'file': 'chapter_5_subsection_4.md'},
    {'pattern': r'Future Directions for Quantum Biology Research in DNA', 'file': 'chapter_5_subsection_5.md'},
    {'pattern': r'Chapter 6 explores the burgeoning field of quantum effects in protein folding and function', 'file': 'chapter_6_subsection_0.md'},
    {'pattern': r'Protein Folding Pathways and Quantum Mechanics', 'file': 'chapter_6_subsection_1.md'},
    {'pattern': r'Quantum Tunneling and Protein Dynamics', 'file': 'chapter_6_subsection_2.md'},
    {'pattern': r'Quantum Fluctuations in Protein Structure and Function', 'file': 'chapter_6_subsection_3.md'},
    {'pattern': r'The Role of Water and Solvent in Quantum Biological Processes', 'file': 'chapter_6_subsection_4.md'},
    {'pattern': r'Applications of Quantum Models in Predicting Protein Structure', 'file': 'chapter_6_subsection_5.md'},
    {'pattern': r'Chapter 7: Applications and Future Directions', 'file': 'chapter_7_subsection_0.md'},
    {'pattern': r'Emerging Applications in Quantum Biology: Biotechnology and Medicine', 'file': 'chapter_7_subsection_1.md'},
    {'pattern': r'Novel Approaches in Quantum Computing for Biological Problems', 'file': 'chapter_7_subsection_2.md'},
    {'pattern': r'Future Research Directions and Open Questions in Quantum Biology', 'file': 'chapter_7_subsection_3.md'},
    {'pattern': r'Ethical Considerations in Quantum Biology Research', 'file': 'chapter_7_subsection_4.md'},
    {'pattern': r'Quantum Biology and the Development of New Technologies', 'file': 'chapter_7_subsection_5.md'},
    {'pattern': r'Concluding Remarks on Quantum Biology\'s Impact on Science', 'file': 'chapter_7_subsection_6.md'},
]

def find_start_line(pattern):
    for i in range(80, len(lines)):
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
    content = lines[start:end]
    print(f"Section: {section['file']}, start: {start}, end: {end}, len(content): {len(content)}")
    with open(section['file'], 'w', encoding='utf-8') as f:
        f.write(''.join(content).strip())
    print(f"Created {section['file']} with {len(content)} lines")

print("Splitting completed.")