"""
Python script to check that Texcraft's implementation of pltotf gives the same
result as Knuth's original version. Run from the root of the Texcraft repo.
"""
import pathlib
import os
import difflib

for pl_path in pathlib.Path("crates/tfm/src").rglob("*.pl"):
    knuth_tfm_path = "/tmp/knuth.tfm"
    os.popen(f"pltotf {pl_path} {knuth_tfm_path}").read()
    knuth_tfm = open(knuth_tfm_path, "rb").read()
    texcraft_tfm_path = "/tmp/texcraft.tfm"
    os.popen(f"cargo run --bin pltotf -- {pl_path} {texcraft_tfm_path}").read()
    texcraft_tfm = open(texcraft_tfm_path, "rb").read()
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
        if i > 60:
            print("(truncating additional diffs...)")
            break
    break
