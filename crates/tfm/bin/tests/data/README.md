# .tfm and .pl data for testing

This directory tree hosts .tfm and .pl files for testing.
For a pair `(file.tfm, file.pl)`, 
    either `file.tfm` is the result of Knuth's `pltotf file.pl`,
    or `file.pl` is the result of Knuth's `tftopl file.tfm`.
The CLI E2E tests then verify that Texcraft's version of these command gives the identical result.

In some cases the roundtrip conversion works - i.e., 
    `file.tfm` is the result of Knuth's `pltotf file.pl`
    _and_ `file.pl` is the result of Knuth's `tftopl file.tfm`.
This happens when both the .tfm and .pl file are in their respective canoncial forms.
In this case the E2E tests verify that both conversion directions works.
While this is in some sense the ideal case, we can't have this for
  every test because we sometimes want to test non-canonical input files
  (like an empty .pl file, which is valid, but not in canonical form).

The input files are sourced online or written by hand.
For files sourced online it is important to attribute the file correctly.

## Adding new test cases

In general there are 5 kinds of test case.

### Input is a 100% valid .tfm file

Add the .tfm file and run `tftopl $file.tfm`,
  verify there is no output on stderr,
  and save the result as `$file.pl`.
Check if `pltotf $file.pl` recovers the original .tfm file.
If so add a roundtrip test; otherwise add a .tfm to .pl test.

### Input is a valid .tfm file, with warnings

Add the .tfm file and run `tftopl $file.tfm`,
  capture the stderr to `$file.stderr.txt`
  and then save the result as `$file.pl`.
Add a .tfm to .pl test.

### Input is an invalid .tfm file

Add the .tfm file and run `tftopl $file.tfm`,
  capture the stderr to `$file.stderr.txt$
  and verify there is no output.
Add a .tfm to .pl test.

### Input is a 100% valid .pl file

Follow the playbook for 100% valid .tfm file,
  swapping `tftopl` and `pltotf`.

### Input is a .pl file with warnings

Follow the playbook for valid .tfm file with warnings,
  swapping `tftopl` and `pltotf`.
