import os
import json
import glob

def create_index():
    folder_path = os.path.dirname(__file__)
    json_files = glob.glob(os.path.join(folder_path, 'chapter_*_subsection_*.json'))

    # Sort files by chapter and subsection
    def file_sort_key(file_path):
        # Extract chapter and subsection numbers
        filename = os.path.basename(file_path)
        # filename like chapter_1_subsection_2_3.json
        parts = filename.replace('chapter_', '').replace('subsection_', '').replace('.json', '').split('_')
        if len(parts) >= 4:
            chapter = int(parts[0])
            subsection_major = int(parts[1])
            subsection_minor = int(parts[2]) if len(parts) > 2 else 0
            subsection_patch = int(parts[3]) if len(parts) > 3 else 0
            return (chapter, subsection_major, subsection_minor, subsection_patch)
        else:
            return (999, 0, 0, 0)

    json_files.sort(key=file_sort_key)

    titles = []
    for json_file in json_files:
        try:
            with open(json_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
                title = data.get('title', '')
                if title:
                    titles.append(title)
        except (json.JSONDecodeError, KeyError):
            print(f"Error reading {json_file}")

    # Write to index.json
    with open(os.path.join(folder_path, 'index.json'), 'w', encoding='utf-8') as f:
        json.dump({"titles": titles}, f, indent=2, ensure_ascii=False)

    print(f"Created index.json with {len(titles)} titles")

if __name__ == "__main__":
    create_index()