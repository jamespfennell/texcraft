# .ftm and .pl data for testing

## Generate a .tfm file and normalize the .pl files

To test an edge case write a basic .pl file and then run the following command
    using Knuth's `pltotf` and `tftopl`:

```
FILE=zero-width-char
pltotf $FILE $FILE && tftopl $FILE $FILE
```

This creates the .tfm file and normalizes the .pl file.
