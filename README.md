## Brainfuck interpreter in haskell

This is a brainfuck interpreter written in haskell.

You can run it with `cabal run`, you'll need cabal and ghc installed. The program will read from stdin, and start interpreting the program once it sees EOF.

You can run the test program like this: `cat goldenratio.b | cabal run`. This should print out the digits of the golden ratio.