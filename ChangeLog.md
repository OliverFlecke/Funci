# Revision history for Compiler

# Todo list: 
- [x] Get the HandParser to return the error messages properly instead of just calling error 
- [ ] Write test cases for the HandParser
    - [x] Add test cases for arithmic expressions
    - [x] Add test cases for boolean expressions
    - [ ] Add test cases for lists 
    - [ ] Add test cases for let expressions 
    - [ ] Add test cases for if then else expressions
- [ ] Implement the HandParser
    - [x] Implement the arithmic language for the parser
    - [x] Implement the boolean language for the parser
    - [x] Implement support for lists with numbers
        - [ ] Upgrade this to support lists of any type
        - [ ] Find a better way to implement the different types
    - [x] Implement let expressions
    - [ ] Implement if then else expressions
    - [ ] Implement case expressions
- [ ] Write documentation about how the language actually works

## Goal - 0.2.0.0
This version of the lexer and parser should be done entirely with the builtin libraries for parsing in Haskell. 

#### 0.1.0.10 -- 2016-12-27
* Support for if-then-else expressions added
* Undid types in list, so list now can contain multiple types, as this should be done when typechecking instead.

#### 0.1.0.9  -- 2016-12-27
* Added functionallity to parse basic functions, such as a main function
* Have restructed some of the syntax, so functions can have a type signature later on. This means that constants (numbers) are a bit long to write out in abstract syntax at the moment. I would like to make that simpler, if possible. 
* The languages of booleans and numbers have been merged via the parseBasic function, to allow for parsing either of them. Not sure how the error messages will be passed along. 

#### 0.1.0.8  -- 2016-12-22
* List made generic - They are at the moment a bit to generic, as one list can contain multiple types, which should not be allowed.
* Rewrite of the syntax - Hopefully this can be cleaned up a bit

#### 0.1.0.7  -- 2016-12-21
* Support for list of numbers added

#### 0.1.0.6  -- 2016-12-21
* Boolean language support added 
    * Lexing boolean constants 
    * Parsing boolean constants
    * Parsing boolean expressions with And, Or, and Not operator
* Let expressions are now handled by the parser

#### 0.1.0.5  -- 2016-12-21 
* Added Comparator and boolean operators to the syntax of the language
* Added tests for lexing comparator operators 
* Added functionallity for lexing comparator operators
* Added test for lexer boolean operators 
* Added functionallity for lexing boolean operators
* Made an explicit function in the parser for only parsing arithmic strings

#### 0.1.0.4  -- 2016-12-20
* Renamed the arithmic operators abstract representation, so they all have the same length (so it is now called Add instead of Plus and so on).
* All the definitions of the syntax has now been moved to the new file \src\HandSyntax.hs
* Defined test for testing the parsing for alrithmic operations. 
* The parser can now handle simple alrithmics with addition, subtraction, multiplcation, and division (rem is still missing). 

#### 0.1.0.3  -- 2016-12-19
Got Hspec and QuickCheck properly installed, so automatic tests can start to be created. Renamed the Lexer.hs to HandLexer.hs, assuming it will later be replaced be a Lexer (or even Parser only) version. 

Tests for simple opertors, numbers, and brackets are included. 

#### 0.1.0.2  -- 2016-12-18
Renamed the compiler to Funci, which will be its working name.

#### 0.1.0.1  -- 2016-12-15
The standard lexer is now working with everything. I'm not quite sure if I have to do anything extra at this level to handle functions, or if I'm able to just do that when building the AST. 

## 0.1.0.0  -- 2016-12-15
* First version. Released on an unsuspecting world.
This does so far contain the lexer and parser files, planning to be handwritten. 
