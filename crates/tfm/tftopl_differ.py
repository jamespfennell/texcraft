"""
Python script to check that Texcraft's implementation of tftopl gives the same
result as Knuth's original version. Run from the root of the Texcraft repo.

rsync -zarvm --include="*/" --include="*.tfm" --exclude="*" rsync://mirrors.mit.edu/CTAN/ .
"""
import pathlib
import sys
import difflib
import subprocess
import argparse


def run(command):
    p = subprocess.run(command, capture_output=True, text=True)
    return p.stdout, p.stderr

parser = argparse.ArgumentParser(
                    prog='tftopl_differ',
                    description="""Finds diffs between Knuth's tftopl and Texcraft's tftopl.
                    It must be run from within the Texcraft repository.
                    Knuth's tftopl must be installed.""")
parser.add_argument('path', default='./')
parser.add_argument('--fail-fast', action='store_true') 
args = parser.parse_args()

for tfm_path in pathlib.Path(args.path).rglob("*.tfm"):
    knuth_pl, knuth_stderr = run(["tftopl", tfm_path])
    texcraft_pl, texcraft_stderr = run(["cargo", "run", "--quiet", "--bin", "tftopl", "--", tfm_path])
    if knuth_pl == texcraft_pl and knuth_stderr == texcraft_stderr:
        print(f"{tfm_path} OK")
        continue
    print(f"{tfm_path} ERROR")
    for line in difflib.unified_diff(knuth_stderr.splitlines(), texcraft_stderr.splitlines(), fromfile='Knuth stderr', tofile='Texcraft stderr', lineterm=''):
        print(line)
    i = 0
    for line in difflib.unified_diff(knuth_pl.splitlines(), texcraft_pl.splitlines(), fromfile='Knuth', tofile='Texcraft', lineterm=''):
        print(line)
        i += 1
        if i > 60:
            print("(truncating additional diffs...)")
            break
    if args.fail_fast:
        break

