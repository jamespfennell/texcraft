"""
Python script to check that Texcraft's implementation of pltotf gives the same
result as Knuth's original version. Run from the root of the Texcraft repo.

rsync -zarvm --include="*/" --include="*.pl" --exclude="*" rsync://mirrors.mit.edu/CTAN/ .
"""
import pathlib
import os
import difflib
import argparse

parser = argparse.ArgumentParser(
                    prog='pltotf_differ',
                    description="""Finds diffs between Knuth's pltotf and Texcraft's pltotf.
                    It must be run from within the Texcraft repository.
                    Knuth's pltotf must be installed.""")
parser.add_argument('path', default='./')
parser.add_argument('--fail-fast', action='store_true') 
args = parser.parse_args()


for pl_path in pathlib.Path(args.path).rglob("*.pl"):
    knuth_tfm_path = "/tmp/knuth.tfm"
    os.popen(f"rm -f {knuth_tfm_path}").read()
    os.popen(f"pltotf {pl_path} {knuth_tfm_path}").read()
    knuth_tfm = open(knuth_tfm_path, "rb").read()
    texcraft_tfm_path = "/tmp/texcraft.tfm"
    os.popen(f"rm -f {texcraft_tfm_path}").read()
    os.popen(f"cargo run --quiet --bin pltotf -- {pl_path} {texcraft_tfm_path}").read()
    try:
        texcraft_tfm = open(texcraft_tfm_path, "rb").read()
    except FileNotFoundError:
        texcraft_tfm = []
    if knuth_tfm == texcraft_tfm:
        print(f"{pl_path} OK")
        continue
    print(f"{pl_path} ERROR")

    knuth_debug = os.popen(f"cargo run --bin tfmtools -- debug {knuth_tfm_path}").read()
    texcraft_debug = os.popen(f"cargo run --bin tfmtools -- debug {texcraft_tfm_path}").read()
    i = 0
    for line in difflib.unified_diff(knuth_debug.splitlines(), texcraft_debug.splitlines(), fromfile='Knuth', tofile='Texcraft', lineterm=''):
        print(line)
        i += 1
        if i > 600:
            print("(truncating additional diffs...)")
            break
    if args.fail_fast:
        break
