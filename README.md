# Genetic Programming - Arithmetic Expressions

## Authors
**Team Name:** DB_BI
- Daniel Bravo - [bravod@csu.fullerton.edu]  
- Barry Ibarra - [barryjr01@csu.fullerton.edu]

## Introduction
**CPSC 481-03 - Artificial Intelligence**  
This is a GP (Genetic Programming) problem. As you know, GP is a variant of the GA (Genetic Algorithm) family, except that 1) it works on programs (each usually represented as a Tree) rather than on DNA gene strings (usually represented as bit strings), and 2) to evaluate an individual's fitness we will "run" the program tree to get a value, the fitness value. We will be using GP to find a good approximation to a goal: an unknown quadratic arithmetic expression in several variables. (If you don't remember what such a thing is, not to worry: the GP program will tell you.)
To simplify things, each individual of our population is an arithmetic expression (an "expr"). An expr can be composed of its "alphabet" of operators, variable names, and constant integers. See Luger, section 12.2.2 for more information.


## Getting Started

### Files
* gp_arith_expr.lisp

### External Requirements
None

### Prerequisites
You will need a Lisp interpreter to run this code.  
On a mac you can install Lisp using brew:

```
brew install clisp
```

### Usage
Make sure you are in the folder containing the file and run:
```
clisp gp_arith_expr.lisp
```
_Note: this has only been tested on MacOS using [GNU CLISP 2.49 (2010-07-07)](http://clisp.cons.org/), but should work with any lisp interpreter._

## Extra Features
None

## Bugs
None
