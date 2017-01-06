# Revision history for Compiler

## 1.0.0.0

This version should contain a fully working compiler for compiling a functional language, capable of handling semilar syntax to haskell. Numbers in this language should also be able to 'unit-check' between units (like meters, seconds, and so on).

### 0.1.4.1   -- 2017-01-06

* SI base units can now be lexed and parsed with numbers.
* An custom unit type is also supported to store any unknow unit as a basic string.
* Standard unit prefixes is supported from yotta to yocto.
* Units with exponents are also handled.

### 0.1.4.0   -- 2017-01-06

* Added metric units to the syntax definition
  * Lexer and Parser should be able to parse some of them, but no prefixes yet
  * Evaluator does not check them yet
* All tests has been updated to handle the new syntax (Note that the units are optional, using the Maybe type)

#### 0.1.3.3  -- 2017-01-05

* Parser can now handle more advanced types that are specified with parenticies
* Added test cases for recursion, which are not all running yet.
* The RunPrograms test suite has been reverted to the old way with hspec, as this has a better output to the console for fixing errors, and will actually make the test suite fail if there is an error.

#### 0.1.3.2  -- 2017-01-04

* Reworte the programTest to run the test with HUnit instead, in order to run lists of tests
* Added lots of test cases

#### 0.1.3.1  -- 2017-01-04

* Unary negation can now be lexer, parsed, and evaluated.
  * This is done with a bit of a hack, as the subtraction operator is overloaded. Maybe this can be improved

### 0.1.3.0   -- 2017-01-04

* Functions can now handle type assignments (Still no type checker)
  * Functions type can be defined as: add x y :: Int -> Int -> Int = x + y

#### 0.1.2.8  -- 2017-01-03

* The overall program can now take a file as commandline argument and run it
* Created a test program to run and test all the programs in the test\programs folder. This test suite is called RunPrograms

#### 0.1.2.7  -- 2017-01-03

* Evaluator now supports functions in let expressions

#### 0.1.2.6  -- 2017-01-03

* Let expressions has been rewriten to use the bind syntax as functions does
* Parser can now parse functions created in let expressions

#### 0.1.2.5  -- 2017-01-02

* Functions can now handle an appitrary number of arguments

#### 0.1.2.4  -- 2017-01-02

* Functions without parameters can now be called

#### 0.1.2.3  -- 2016-12-29

* If-then-else expressions can now be evaluated

#### 0.1.2.2  -- 2016-12-29

* Fixed parser error when parsing let expressions with multiple variables
* Evaluator can now evaluate let expressions

#### 0.1.2.1  -- 2016-12-29

* Parser now handles list correctly.
  * The sugar syntax is not handle with the current version (that is [1:2:3])
  * Arithmics inside the list are being correctly parsed as well
* Evaluator correctly evaluating list
  * Arithmics inside the lists are being fully evaluated as well (1+2 : [] would be [3])

### 0.1.2.0   -- 2016-12-28

* The structure of how the operators are parsed have been completly changed. Every operator is now defined in the same language, where they follow the standard precedence order (from C and most programming languages). This has made it lot more clear what level each operator is working on.
  * Note: No new operators have been added at this stage, so some of the precedence levels are not in use currently

#### 0.1.1.3  -- 2016-12-28

* Added support for the comparator operators in the parser
* Evaluator can now handle compartor operators between numbers
  * These are currently at the same priority level as Add and Sub operator
  * They cannot currently be interleaved with standard boolean values

#### 0.1.1.2  -- 2016-12-28

* Renamed the reminder operator to modulo ('Rem' to 'Mod')
* Evaluator can now evaluate boolean expressions

#### 0.1.1.1  -- 2016-12-28

* The reminder operator is added to the parser
* Error when parsing parentizies (Had mixed up the left and right)

### 0.1.1.0   -- 2016-12-27

* Evaluator now has defined functions for evaluating programs.
  * Evaluator can evauale basic arithmics
* Basic tests for the evaluator has been created
* Renamed the files and modules to not having the 'Hand' in every name

#### 0.1.0.11 -- 2016-12-27

* Let statements can now handle multiple assignments in one

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

* Got Hspec and QuickCheck properly installed, so automatic tests can start to be created. Renamed the Lexer.hs to HandLexer.hs, assuming it will later be replaced be a Lexer (or even Parser only) version.
* Tests for simple opertors, numbers, and brackets are included.

#### 0.1.0.2  -- 2016-12-18

Renamed the compiler to Funci, which will be its working name.

#### 0.1.0.1  -- 2016-12-15

The standard lexer is now working with everything. I'm not quite sure if I have to do anything extra at this level to handle functions, or if I'm able to just do that when building the AST.

## 0.1.0.0  -- 2016-12-15

First version. Released on an unsuspecting world.
This does so far contain the lexer and parser files, planning to be handwritten.
