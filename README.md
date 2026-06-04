## Brainfuck interpreter in haskell

This is a brainfuck interpreter written in haskell.

### Getting Started

You'll need ghc and cabal installed on your system.

To interpret brainfuck code directly:

```
cat goldenratio.b | cabal run -- ctobf
```

To compile brainfuck code to C:

```
cat goldenratio.b | cabal run -- ctobf -c
```

To save it to a file instead of printing to standard output:

```
cat goldenratio.b | cabal run -- ctobf -c -o out.c
```
