# Compiler for Funci, a functional language

This project is focus on building a functional language. So far, I'm only starting on the lexer and parser, which will first be build by hand, and afterwards by some of the inbuild parser libraries in Haskell.


## Setup and install

In order to build and run this project Haskell 8.0 and Cabal is required. Cabal is used to build the project.

After having cloned the project onto your local system, run the following commands from the command line:

    cabal configure
    cabal build

The executable will then be located at \dist\build\Funci\Funci

## Testing

The testing of each module is done by its corrisponding Test*.hs file in the \test\ directory. All the tests can be run with:

    cabal test

Note: Tests are not enabled by default. To enable test run:

    cabal configure --enable-tests

Individual test suits can be run with:

    cabal test [TEST_SUIT_NAME]