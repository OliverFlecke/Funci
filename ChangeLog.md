# Revision history for Compiler

## Goal - 0.2.0.0
This version of the lexer and parser should be done entirely with the builtin libraries for parsing in Haskell. 

### 0.1.0.3  -- 2016-12-18
* Got Hspec and QuickCheck properly installed, so automatic tests can start to be created. Renamed the Lexer.hs to HandLexer.hs, assuming it will later be replaced be a Lexer (or even Parser only) version. 

Tests for simple opertors, numbers, and brackets are included. 

### 0.1.0.2  -- 2016-12-18
* Renamed the compiler to Funci, which will be its working name.

### 0.1.0.1  -- 2016-12-15
The standard lexer is now working with everything. I'm not quite sure if I have to do anything extra at this level to handle functions, or if I'm able to just do that when building the AST. 

## 0.1.0.0  -- 2016-12-15

* First version. Released on an unsuspecting world.
This does so far contain the lexer and parser files, planning to be handwritten. 
