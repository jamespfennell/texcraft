# .tfm and .pl data for testing

This directory hosts pairs of .tfm and .pl files for testing.
In general for a pair `(file.tfm, file.pl)`, 
    one of the files will have been sourced from e.g. TeX or written by hand,
    and then the other pair will have been generated by Knuth's tftopl or pltotf programs.
The unit tests in the parent directory validate that Texcraft performs the identical conversion.

Many of the unit tests validate that the conversion works both ways;
    e.g. that Texcraft converts `file.tfm` to `file.pl`
    and converts `file.pl` to `file.tfm`.
One tricky detail here is that multiple .pl files can lead to the same .tfm.
For these tests to work it's necessary to commit the "normalized" .pl
    that is outputted by Knuths tftopl program.
This can be done by doing this:

```
FILE=cmr10
pltotf $FILE $FILE && tftopl $FILE $FILE
```