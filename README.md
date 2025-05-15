# Code-ception: The Hello World That Writes Itself 🤯

Welcome to **Code-ception**, the most gloriously unhinged "Hello, World!" you'll ever encounter. This Python script writes a C++ program, which writes *another* Python script to print a multilingual "Hello, World!"—complete with ASCII art and a cosmic chart. Why? Because a simple `echo Hello World` is for amateurs, and we’re here for the chaos. Crafted with pride in Lowell, Massachusetts—*sláinte* (Irish), *sousdey* (Khmer), *síu sām* (Cantonese), and *konnichiwa* (Japanese).

## What's This Nonsense?

This is a Python → C++ → Python pipeline that’s absurdly overengineered for maximum comedic effect:
- **`code_ception.py`**: The mastermind Python script that:
  1. Generates a C++ program (`generate_hello.py.cpp`).
  2. Compiles it with `g++`.
  3. Runs it to birth `hello_world.py`.
  4. Executes `hello_world.py` to display a random multilingual greeting, ASCII art, and a starry Carl Sagan quote.
  5. Prints a cosmic chart of all greetings for extra flair.
- **`generate_hello.py.cpp`**: A C++ program that creates `hello_world.py` with a randomly chosen greeting in English, Irish Gaelic, Khmer, Cantonese, Spanish, or Japanese.
- **`hello_world.py`**: The final Python script that:
  - Prints “HELLO” in blocky ASCII art (via `pyfiglet`, optional).
  - Outputs a greeting like “こんにちは、世界！ | KON-NEE-chee-wah SEH-kai | Japanese Hello World”.
  - Could’ve been a one-liner, but where’s the fun in that?

Expect greetings in multiple languages, simplified phonetics for coders brave enough to pronounce them, and a chart that pretends to organize the chaos. It’s Python → C++ → Python, layered with cosmic absurdity and a nod to Carl Sagan’s star-stuff wisdom.

## Why So Absurd?

Because coding is a comedy show, and we’re the headliners. This script could’ve been `print("Hello, World!")`, but instead, we built a multilingual, ASCII-art-fueled, cosmic-chart-topping monster. Made in Lowell, MA, where the Merrimack River inspires such madness.

## How to Witness the Madness

### Prerequisites
- **Python 3.x**: You’ve got this.
- **g++**: Available on Linux/macOS (e.g., Ubuntu). On Windows, use MinGW or WSL (untested, proceed with caution).
- **Optional**: Install `pyfiglet` for ASCII art (`pip install pyfiglet`). Without it, you’ll get a simple `===== HELLO =====` banner.
- **Ubuntu Recommended**: Tested on Ubuntu. Windows support is theoretical—Windows CMD and `g++` might need extra love for Unicode and compilation.

### Steps
1. Clone this repo (you know you want to):
   ```bash
   git clone https://github.com/your-username/code-ception.git
   cd code-ception
   ```

2. Run the script and brace for absurdity:
   ```bash
   python3 code_ception.py
   ```

3. Watch the magic unfold:
   - The script generates and compiles `generate_hello.py.cpp`.
   - It creates `hello_world.py`, which prints a random greeting (e.g., “Sláinte to the World! | SLAWN-chuh tuh thuh WURLD | Irish Gaelic cheers to the world”).
   - ASCII art announces “HELLO” (or a fallback banner if `pyfiglet` is missing).
   - A Carl Sagan quote reminds you we’re all star-stuff.
   - A cosmic chart lists all greetings, from English to Japanese.

### Example Output
```plaintext
C++ compilation successful! The cosmos approves!
C++ executable ran successfully! Stars align!
Generated hello_world.py content:
#!/usr/bin/env python3
# Optional: Install pyfiglet for ASCII art (pip install pyfiglet)
try:
    from pyfiglet import Figlet
    f = Figlet(font='standard')
    print(f.renderText('HELLO'))
except ImportError:
    print('===== HELLO =====')
print("こんにちは、世界！ | KON-NEE-chee-wah SEH-kai | Japanese Hello World")

   _          _ _       
  | |__   ___| | | ___  
  | '_ \ / __| | |/ _ \ 
  | | | | (__| | | (_) |
  |_| |_| \___|_|_|\___/
こんにちは、世界！ | KON-NEE-chee-wah SEH-kai | Japanese Hello World

We are all made of star-stuff. We are a way for the universe to know itself.” — Carl Sagan

Cosmic Greetings Chart:
----------------------------------------------------------------------------------------------------
Greeting                             | Phonetics                 | Description                   
----------------------------------------------------------------------------------------------------
Hello, World!                       | N/A                       | English greeting              
Salutations, Cosmos!                | N/A                       | English cosmic greeting       
Greetings, Multiverse!              | N/A                       | English multiversal greeting  
Yo, Universe, What's Good?          | N/A                       | English slang greeting        
Sláinte to the World!               | SLAWN-chuh tuh thuh WURLD | Irish Gaelic cheers to the world
Sousdey លោក!                      | SOOS-day LOHK             | Khmer Hello World             
Síu sām 世界!                       | SYOO SUM SAI GAI          | Cantonese Hello World         
¡Hola, Mundo!                       | OH-lah MOON-doh           | Spanish Hello World           
こんにちは、世界！                     | KON-NEE-chee-wah SEH-kai  | Japanese Hello World          
----------------------------------------------------------------------------------------------------
```
## Optional
   ```bash
   python3 code-cobol.py
   ```
   or (to install GnuCOBOL)
   ```bash
   sudo python3 code-cobol.py
   ```

### Output
```plaintext
GnuCOBOL installed successfully!
Created hello.cob successfully!
Compiling hello.cob...
hello.cob:1: note: free format detected
    1 > IDENTIFICATION DIVISION.
    2 | PROGRAM-ID. HelloWorld.
    3 | PROCEDURE DIVISION.
<command-line>: warning: "_FORTIFY_SOURCE" redefined
<command-line>: note: this is the location of the previous definition
Running hello.cob...
Hello, World!
Done! Enjoy your COBOL Hello World!
```

## Notes
- **Unicode**: Greetings include Khmer (លោក), Cantonese (世界), and Japanese (世界). Ensure your terminal supports UTF-8. Windows CMD may struggle with non-ASCII characters.
- **Windows**: Untested on Windows. You’ll need `g++` (via MinGW/WSL) and may need to tweak the C++ executable name (`generate_hello.exe`). Feedback welcome!
- **Absurdity**: This is Python generating C++ generating Python, all to avoid a one-line `print`. Revel in the chaos.

## Contributing
Got a wilder way to say “Hello, World!”? Add a new language, tweak the ASCII art, or make it even more absurd. Open a PR and join the code-ception party!

## License
MIT License—fork, modify, and spread the code-ception madness!

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Made with 💥 in Lowell, Massachusetts.

