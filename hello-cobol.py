import subprocess
import os

def install_gnucobol():
    """Install GnuCOBOL using apt."""
    try:
        print("Updating apt package list...")
        subprocess.run(["sudo", "apt", "update"], check=True)
        print("Installing gnucobol...")
        subprocess.run(["sudo", "apt", "install", "-y", "gnucobol"], check=True)
        print("GnuCOBOL installed successfully!")
    except subprocess.CalledProcessError as e:
        print(f"Error installing GnuCOBOL: {e}")
        exit(1)

def write_cobol_file():
    """Write COBOL Hello World to hello.cob."""
    cobol_code = """IDENTIFICATION DIVISION.
PROGRAM-ID. HelloWorld.
PROCEDURE DIVISION.
    DISPLAY 'Hello, World!'.
    STOP RUN.
"""
    try:
        with open("hello.cob", "w") as f:
            f.write(cobol_code)
        print("Created hello.cob successfully!")
    except IOError as e:
        print(f"Error writing hello.cob: {e}")
        exit(1)

def compile_and_run():
    """Compile and run hello.cob."""
    try:
        print("Compiling hello.cob...")
        subprocess.run(["cobc", "-x", "hello.cob"], check=True)
        print("Running hello.cob...")
        subprocess.run(["./hello"], check=True)
    except subprocess.CalledProcessError as e:
        print(f"Error compiling/running hello.cob: {e}")
        exit(1)

if __name__ == "__main__":
    print("Starting COBOL Hello World automation...")
    if not os.path.exists("/usr/bin/cobc"):
        install_gnucobol()
    else:
        print("GnuCOBOL already installed!")
    write_cobol_file()
    compile_and_run()
    print("Done! Enjoy your COBOL Hello World!")