#!/usr/bin/env python3
import os
import subprocess
import random

# List of absurd multilingual greetings with native script, simple phonetics, and translation
greetings = [
    "Hello, World! | English greeting",
    "Salutations, Cosmos! | English cosmic greeting",
    "Greetings, Multiverse! | English multiversal greeting",
    "Yo, Universe, What's Good? | English slang greeting",
    "Sláinte to the World! | SLAWN-chuh tuh thuh WURLD | Irish Gaelic cheers to the world",
    "Sousdey លោក! | SOOS-day LOHK | Khmer Hello World",
    "Síu sām 世界! | SYOO SUM SAI GAI | Cantonese Hello World",
    "¡Hola, Mundo! | OH-lah MOON-doh | Spanish Hello World",
    "こんにちは、世界！ | KON-NEE-chee-wah SEH-kai | Japanese Hello World"
]

# C++ code as a string with a random greeting and ASCII art via pyfiglet with fallback
cpp_code = f"""
#include <fstream>
#include <string>

int main() {{
    std::ofstream outFile("hello_world.py");
    
    if (!outFile) {{
        return 1; // Exit if file creation fails
    }}
    
    outFile << "#!/usr/bin/env python3\\n";
    outFile << "# Optional: Install pyfiglet for ASCII art (pip install pyfiglet)\\n";
    outFile << "try:\\n";
    outFile << "    from pyfiglet import Figlet\\n";
    outFile << "    f = Figlet(font='standard')\\n";
    outFile << "    print(f.renderText('HELLO'))\\n";
    outFile << "except ImportError:\\n";
    outFile << "    print('===== HELLO =====')\\n";
    outFile << "print(\\\"{random.choice(greetings)}\\\")\\n";
    
    outFile.close();
    return 0;
}}
"""

# Write the C++ code to a file
with open("generate_hello.py.cpp", "w") as cpp_file:
    cpp_file.write(cpp_code)

# Compile the C++ code
try:
    compile_result = subprocess.run(
        ["g++", "generate_hello.py.cpp", "-o", "generate_hello"],
        check=True,
        capture_output=True,
        text=True
    )
    print("C++ compilation successful! The cosmos approves!")
except subprocess.CalledProcessError as e:
    print(f"Compilation failed: The galaxy weeps! {e.stderr}")
    exit(1)

# Run the compiled C++ executable
try:
    run_result = subprocess.run(
        ["./generate_hello"],
        check=True,
        capture_output=True,
        text=True
    )
    print("C++ executable ran successfully! Stars align!")
except subprocess.CalledProcessError as e:
    print(f"Execution failed: The void consumes us! {e.stderr}")
    exit(1)

# Verify the generated hello_world.py file
if os.path.exists("hello_world.py"):
    with open("hello_world.py", "r") as py_file:
        print("Generated hello_world.py content:")
        print(py_file.read())
else:
    print("Failed to generate hello_world.py: Reality unravels!")
    exit(1)

# Run hello_world.py
try:
    run_result = subprocess.run(
        ["python3", "hello_world.py"],
        check=True,
        capture_output=True,
        text=True
    )
    print(f"{run_result.stdout.strip()}\n\nWe are all made of star-stuff. We are a way for the universe to know itself.” — Carl Sagan")
except subprocess.CalledProcessError as e:
    print(f"Process failed: Chaos reigns! {e.stderr}")
    exit(1)

# Generate a text-based chart of all greetings
print("\nCosmic Greetings Chart:")
print("-" * 100)
print(f"{'Greeting':<35} | {'Phonetics':<25} | {'Description':<30}")
print("-" * 100)
for greeting in greetings:
    parts = greeting.split(" | ")
    if len(parts) == 2:
        native, desc = parts
        phonetics = "N/A"
    else:
        native, phonetics, desc = parts
    print(f"{native:<35} | {phonetics:<25} | {desc:<30}")
print("-" * 100)