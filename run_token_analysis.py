import subprocess
import os
import re
import sys

# Define the directories to analyze
DIRECTORIES_TO_ANALYZE = [
    "ico-sol",
    "agent-chats",
    "books",
    "kinematics",
    "papers"
]

# Define the output file
OUTPUT_FILE = "token_analysis_results.txt"

# Define the path to the python executable in the virtual environment
# Use sys.executable if the script is run with the venv's python,
# otherwise construct the path relative to the script's location.
VENV_PYTHON_PATH = os.path.join(".venv", "Scripts", "python.exe")
if not os.path.exists(VENV_PYTHON_PATH):
    print(f"Error: Virtual environment Python not found at {VENV_PYTHON_PATH}")
    print("Please ensure the virtual environment exists and the script is run from the project root.")
    sys.exit(1)

# Define the path to the estimation script
ESTIMATION_SCRIPT = "estimate_tokens.py"
if not os.path.exists(ESTIMATION_SCRIPT):
    print(f"Error: Estimation script not found at {ESTIMATION_SCRIPT}")
    sys.exit(1)


def run_estimation(directory):
    """Runs the estimate_tokens.py script for a given directory and returns the output."""
    if not os.path.isdir(directory):
        print(f"Warning: Directory '{directory}' not found. Skipping.")
        return None, f"Directory '{directory}' not found."

    command = [VENV_PYTHON_PATH, ESTIMATION_SCRIPT, directory]
    print(f"Running analysis for: {directory}...")
    try:
        # Use text=True for easier handling of output, capture_output for stdout/stderr
        result = subprocess.run(command, capture_output=True, text=True, check=True, encoding='utf-8')
        print(f"Finished analysis for: {directory}")
        return result.stdout, None
    except FileNotFoundError:
        error_msg = f"Error: Command not found. Ensure '{VENV_PYTHON_PATH}' and '{ESTIMATION_SCRIPT}' exist."
        print(error_msg)
        return None, error_msg
    except subprocess.CalledProcessError as e:
        error_msg = f"Error running script for '{directory}':\n{e.stderr}"
        print(error_msg)
        return None, error_msg
    except Exception as e:
        error_msg = f"An unexpected error occurred for '{directory}': {e}"
        print(error_msg)
        return None, error_msg

def parse_token_count(output):
    """Parses the total token count from the script's output."""
    # Regex to find the line "Total estimated tokens in '...': ..."
    # It captures the number at the end of the line.
    match = re.search(r"Total estimated tokens in '[^']+':\s*(\d+)", output)
    if match:
        return int(match.group(1))
    else:
        # Fallback: try finding the last number in the output if the primary pattern fails
        numbers = re.findall(r'\d+', output)
        if numbers:
            print("Warning: Could not parse token count using primary pattern. Using last number found.")
            return int(numbers[-1])
        else:
            print("Warning: Could not parse token count from output.")
            return None


def main():
    results = {}
    errors = {}
    grand_total = 0

    print("Starting token analysis for specified directories...")

    for directory in DIRECTORIES_TO_ANALYZE:
        output, error = run_estimation(directory)
        if error:
            errors[directory] = error
            continue
        if output:
            token_count = parse_token_count(output)
            if token_count is not None:
                results[directory] = token_count
                grand_total += token_count
            else:
                errors[directory] = "Could not parse token count from output."
                print(f"--- Output for {directory} ---")
                print(output)
                print("--------------------------")


    print(f"\nWriting results to {OUTPUT_FILE}...")
    try:
        with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
            f.write("Token Analysis Results\n")
            f.write("======================\n\n")
            for directory, count in results.items():
                f.write(f"{directory}: {count:,} tokens\n") # Format with commas

            f.write("\n----------------------\n")
            f.write(f"Grand Total: {grand_total:,} tokens\n") # Format with commas

            if errors:
                f.write("\n======================\n")
                f.write("Errors Encountered\n")
                f.write("======================\n\n")
                for directory, error_msg in errors.items():
                    f.write(f"Directory: {directory}\nError: {error_msg}\n\n")
        print("Results successfully written.")
    except Exception as e:
        print(f"Error writing results to file: {e}")

if __name__ == "__main__":
    main()