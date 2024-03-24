"""
Python script to check that Texcraft's implementation of pltotf gives the same
result as Knuth's original version. Run from the root of the Texcraft repo.

To download all .pl files from CTAN:

    rsync -zarvm --include="*/" --include="*.pl" --exclude="*" rsync://mirrors.mit.edu/CTAN/ .

Unfortunately this also downloads a bunch of Perl scripts, so this differ is very noisy.
An alternative method is to download a bunch of .tfm files, using e.g. the command in tftopl_differ.py,
run the other differ with the --save-pl, which results in a corpus of .pl files.
This corpus will tend to be homogenous though and won't have many errors in it.
"""
import pathlib
import os
import difflib
import argparse
import subprocess


def run(command):
    p = subprocess.run(command, capture_output=True, text=True)
    return p.stdout, p.stderr


parser = argparse.ArgumentParser(
                    prog='pltotf_differ',
                    description="""Finds diffs between Knuth's pltotf and Texcraft's pltotf.
                    It must be run from within the Texcraft repository.
                    Knuth's pltotf must be installed.""")
parser.add_argument('path', default='./')
parser.add_argument('--fail-fast', action='store_true') 
parser.add_argument('--delete', action='store_true')
args = parser.parse_args()


for pl_path in pathlib.Path(args.path).rglob("*.pl"):
    # Run Knuth
    knuth_tfm_path = "/tmp/knuth.tfm"
    os.popen(f"rm -f {knuth_tfm_path}").read()
    knuth_stdout, knuth_stderr = run(["pltotf", pl_path, knuth_tfm_path])
    os.popen(f"pltotf {pl_path} {knuth_tfm_path}").read()
    knuth_tfm = open(knuth_tfm_path, "rb").read()

    # Run Texcraft
    texcraft_tfm_path = "/tmp/texcraft.tfm"
    os.popen(f"rm -f {texcraft_tfm_path}").read()
    texcraft_stdout, texcraft_stderr = run(["cargo", "run", "--quiet", "--bin", "pltotf", pl_path, texcraft_tfm_path])
    try:
        f= open(texcraft_tfm_path, "rb")
        texcraft_tfm = f.read()
        f.close()
    except FileNotFoundError:
        print(f"Texcraft did not produce an output file! Stderr: {texcraft_stderr}")
        texcraft_tfm = []

    # Compare output
    if knuth_tfm == texcraft_tfm and knuth_stderr == texcraft_stderr:
        print(f"{pl_path} OK")
        if args.delete:
            os.remove(pl_path)
        continue
    print(f"{pl_path} ERROR")

    # Print stderr diff
    i = 0
    for line in difflib.unified_diff(knuth_stderr.splitlines(), texcraft_stderr.splitlines(), fromfile='Knuth stderr', tofile='Texcraft stderr', lineterm=''):
        print(line)
        i += 1
        if i > 60:
            print("(truncating additional stderr diffs...)")
            break

    # Print output diff
    knuth_debug = os.popen(f"cargo run --quiet --bin tfmtools -- debug --omit-tfm-path {knuth_tfm_path}").read()
    texcraft_debug = os.popen(f"cargo run --quiet --bin tfmtools -- debug --omit-tfm-path {texcraft_tfm_path}").read()
    i = 0
    for line in difflib.unified_diff(knuth_debug.splitlines(), texcraft_debug.splitlines(), fromfile='Knuth', tofile='Texcraft', lineterm=''):
        print(line)
        i += 1
        if i > 60:
            print("(truncating additional output diffs...)")
            break
    if args.fail_fast:
        break
