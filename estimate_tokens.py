import os
import argparse
import tiktoken

# Use cl100k_base encoding, common for GPT-3.5/4 models
# This provides a reasonable estimate for Gemini tokens as well.
try:
    encoding = tiktoken.get_encoding("cl100k_base")
except Exception as e:
    print(f"Error getting tiktoken encoding: {e}")
    # Fallback or exit if necessary, depending on requirements
    # For now, we might try another common one or exit
    try:
        encoding = tiktoken.get_encoding("gpt2") # Fallback
        print("Using fallback encoding 'gpt2'.")
    except Exception as e_fallback:
        print(f"Error getting fallback encoding: {e_fallback}")
        exit(1)


def count_tokens(text):
    """Counts tokens in a string using the loaded tiktoken encoding."""
    if not text:
        return 0
    try:
        tokens = encoding.encode(text)
        return len(tokens)
    except Exception as e:
        print(f"Error encoding text: {e}")
        return 0

def estimate_tokens_in_file(file_path):
    """Estimates tokens for a single file."""
    total_tokens = 0
    skipped = False
    print(f"Starting token estimation for file: {file_path}")
    try:
        # Try reading with UTF-8 first
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        file_tokens = count_tokens(content)
        total_tokens += file_tokens
    except UnicodeDecodeError:
        # If UTF-8 fails, try latin-1
        try:
            with open(file_path, 'r', encoding='latin-1') as f:
                content = f.read()
            file_tokens = count_tokens(content)
            total_tokens += file_tokens
        except Exception as e_read_alt:
            print(f"Warning: Could not read file {file_path} with UTF-8 or latin-1. Skipping. Error: {e_read_alt}")
            skipped = True
    except Exception as e_read:
        print(f"Warning: Could not process file {file_path}. Skipping. Error: {e_read}")
        skipped = True

    if not skipped:
        print(f"\nEstimation complete.")
        print(f"Total estimated tokens in '{file_path}': {total_tokens}")
    else:
        print(f"\nEstimation failed for '{file_path}'.")
        return None
    return total_tokens


def estimate_tokens_in_path(target_path):
    """Recursively estimates tokens for all files in a directory or a single file."""
    total_tokens = 0
    processed_files = 0
    skipped_files = 0

    if not os.path.exists(target_path):
        print(f"Error: Path not found at {target_path}")
        return None

    if os.path.isdir(target_path):
        print(f"Starting token estimation in directory: {target_path}")
        for root, _, files in os.walk(target_path):
            for filename in files:
                file_path = os.path.join(root, filename)
            try:
                # Try reading with UTF-8 first, common for text files
                with open(file_path, 'r', encoding='utf-8') as f:
                    content = f.read()
                file_tokens = count_tokens(content)
                # print(f" - {file_path}: {file_tokens} tokens") # Uncomment for detailed per-file count
                total_tokens += file_tokens
                processed_files += 1
            except UnicodeDecodeError:
                # If UTF-8 fails, try latin-1 or skip
                try:
                    with open(file_path, 'r', encoding='latin-1') as f:
                        content = f.read()
                    file_tokens = count_tokens(content)
                    # print(f" - {file_path} (latin-1): {file_tokens} tokens") # Uncomment for detailed per-file count
                    total_tokens += file_tokens
                    processed_files += 1
                except Exception as e_read_alt:
                    print(f"Warning: Could not read file {file_path} with UTF-8 or latin-1. Skipping. Error: {e_read_alt}")
                    skipped_files += 1
            except Exception as e_read:
                print(f"Warning: Could not process file {file_path}. Skipping. Error: {e_read}")
                skipped_files += 1

        print(f"\nEstimation complete for directory.")
        print(f"Processed {processed_files} files.")
        if skipped_files > 0:
            print(f"Skipped {skipped_files} files due to reading errors.")
        print(f"\nTotal estimated tokens in directory '{target_path}': {total_tokens}")
        return total_tokens
    elif os.path.isfile(target_path):
        return estimate_tokens_in_file(target_path)
    else:
        print(f"Error: Path '{target_path}' is neither a file nor a directory.")
        return None


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Estimate LLM tokens in a file or all files within a directory using tiktoken.")
    parser.add_argument("path", help="The path to the file or directory to scan.")
    args = parser.parse_args()

    estimate_tokens_in_path(args.path)