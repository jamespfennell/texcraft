"""
Python script to check that Texcraft's implementation of tftopl gives the same
result as Knuth's original version. Run from the root of the Texcraft repo.
"""
import pathlib
import os
import difflib

for tfm_path in pathlib.Path("crates/tfm/src").rglob("*.tfm"):
    knuth_pl = os.popen(f"tftopl {tfm_path}").read()
    texcraft_pl = os.popen(f"cargo run --bin tftopl -- {tfm_path}").read()
    if knuth_pl == texcraft_pl:
        print(f"{tfm_path} OK")
        continue
    print(f"{tfm_path} ERROR")
    i = 0
    for line in difflib.unified_diff(knuth_pl.splitlines(), texcraft_pl.splitlines(), fromfile='Knuth', tofile='Texcraft', lineterm=''):
        print(line)
        i += 1
        if i > 60:
            print("(truncating additional diffs...)")
            break
    break
