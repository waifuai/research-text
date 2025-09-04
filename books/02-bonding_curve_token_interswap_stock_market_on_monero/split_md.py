import os

folder = "02-bonding_curve_token_interswap_stock_market_on_monero"
file_path = os.path.join(folder, "02-bonding_curve_token_interswap_stock_market_on_monero.md")

with open(file_path, 'r', encoding='utf-8') as f:
    lines = f.readlines()

# Sections: list of (chapter, sub, start_line, end_line)
sections = [
    (1, 0, 97, 102),
    (1, 1, 102, 136),
    (1, 2, 136, 182),
    (1, 3, 182, 223),
    (1, 4, 223, 258),
    (1, 5, 258, 294),
    (1, 6, 294, 336),
    (2, 0, 336, 341),
    (2, 1, 341, 456),
    (2, 2, 456, 526),
    (2, 3, 526, 567),
    (2, 4, 567, 609),
    (3, 0, 609, 614),
    (3, 1, 614, 658),
    (3, 2, 658, 703),
    (3, 3, 703, 743),
    (3, 4, 743, 808),
    (4, 0, 818, 822),
    (4, 1, 822, 861),
    (4, 2, 861, 912),
    (4, 3, 912, 962),
    (4, 4, 962, 1011),
    (4, 5, 1011, 1070),
    (5, 0, 1070, 1076),
    (5, 1, 1076, 1133),
    (5, 2, 1133, 1180),
    (5, 3, 1180, 1245),
    (5, 4, 1245, 1298),
    (5, 5, 1298, 1337),
    (6, 0, 1337, 1346),
    (6, 1, 1346, 1380),
    (6, 2, 1380, 1433),
    (6, 3, 1433, 1475),
    (6, 4, 1475, 1514),
    (6, 5, 1514, 1590),
    (7, 0, 1590, 1592),
    (7, 1, 1592, 1679),
    (7, 2, 1679, 1780),
    (7, 3, 1780, 1796),
    (7, 4, 1796, 1819),
    (7, 5, 1819, 1908),
    (8, 0, 1908, 1910),
    (8, 1, 1910, 1949),
    (8, 2, 1949, 1973),
    (8, 3, 1973, 2030),
    (8, 4, 2030, 2059),
    (8, 5, 2059, 2108),
    (9, 0, 2108, 2149),
    (9, 1, 2149, 2196),
    (9, 2, 2196, 2240),
    (9, 3, 2240, 2321),
]

for ch, sub, start, end in sections:
    content_lines = lines[start-1:end-1]  # Python 0-based indexing
    content = ''.join(content_lines)
    if sub > 0:
        # Replace the first header from ## or ### to #
        content_lines = content.split('\n')
        if content_lines and content_lines[0].strip().startswith('### '):
            content_lines[0] = '#' + content_lines[0].strip()[3:]
        elif content_lines and content_lines[0].strip().startswith('## '):
            content_lines[0] = '#' + content_lines[0].strip()[2:]
        content = '\n'.join(content_lines)
    filename = os.path.join(folder, f'chapter_{ch}_subsection_{sub}.md')
    with open(filename, 'w', encoding='utf-8') as f:
        f.write(content)

# Now, delete the original
os.remove(file_path)