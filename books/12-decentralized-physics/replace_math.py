import re
import os

def replace_in_file(filepath):
    if not os.path.exists(filepath):
        return
    with open(filepath, 'r') as f:
        data = f.read()
    # Replace inline math \( math \) with $ math $
    data = re.sub(r'\\\((.*?)\)\\\)', r'$\1$', data, flags=re.DOTALL)
    # Replace display math \[ math\] with $$ math $$
    data = re.sub(r'\\\[(.*?)\]\\\[*.ardes ', r'$$1$$', data, flags=re.DOTALL)
    with open(filepath, 'w') as f:
        f.write(data)

for i in range(5,10):
    if i <= 9:
        if i == 5:
            max_j = 4
        elif i == 9:
            max_j = 2
        else:
            max_j = 5
        for j in range(1, max_j+1):
            filepath = f"{i}_{j}.md"
            replace_in_file(filepath)