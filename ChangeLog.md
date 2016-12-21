# Revision history for Compiler

# Todo list: 
* Get the HandParser to return the error messages properly instead of just calling error 
* Write test cases for the HandParser

## Goal - 0.2.0.0
This version of the lexer and parser should be done entirely with the builtin libraries for parsing in Haskell. 

### 0.1.0.6  -- 2016-12-21
* Boolean language support added 
** Lexing boolean constants 
** Parsing boolean constants
** Parsing boolean expressions with And, Or, and Not operator
* Let expressions are now handled by the parser

### 0.1.0.5  -- 2016-12-21 
* Added Comparator and boolean operators to the syntax of the language
* Added tests for lexing comparator operators 
* Added functionallity for lexing comparator operators
* Added test for lexer boolean operators 
* Added functionallity for lexing boolean operators
* Made an explicit function in the parser for only parsing arithmic strings

### 0.1.0.4  -- 2016-12-20
* Renamed the arithmic operators abstract representation, so they all have the same length (so it is now called Add instead of Plus and so on).
* All the definitions of the syntax has now been moved to the new file \src\HandSyntax.hs
* Defined test for testing the parsing for alrithmic operations. 
* The parser can now handle simple alrithmics with addition, subtraction, multiplcation, and division (rem is still missing). 

### 0.1.0.3  -- 2016-12-19
* Got Hspec and QuickCheck properly installed, so automatic tests can start to be created. Renamed the Lexer.hs to HandLexer.hs, assuming it will later be replaced be a Lexer (or even Parser only) version. 

Tests for simple opertors, numbers, and brackets are included. 

### 0.1.0.2  -- 2016-12-18
* Renamed the compiler to Funci, which will be its working name.

### 0.1.0.1  -- 2016-12-15
The standard lexer is now working with everything. I'm not quite sure if I have to do anything extra at this level to handle functions, or if I'm able to just do that when building the AST. 

## 0.1.0.0  -- 2016-12-15

* First version. Released on an unsuspecting world.
This does so far contain the lexer and parser files, planning to be handwritten. 
