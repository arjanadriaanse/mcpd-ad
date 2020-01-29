# Description
This code implements an automatic differentiation algorithm known as forward mode.
It operates on a small functional language.


# How to calculate my derivatives?
From the `Main.hs` module, the function `differentiateWrapper` can be run.
This function takes an expression, possibly a function, and evaluates it given a list of arguments.
Since we are talking derivatives, these arguments should all be pairs.
There are some example Wrapper calls in `Main.hs`. 

The function `differentiateWrapper` prints results both for exact differentiation using forward mode, and approximate differentiation using finite differences. 


# Code Structure
To just run examples, `Main.hs` should be sufficient. 
Here, a wrapper is defined that calculates the derivative in multiple ways and prints the result.

`Language.hs` defines the language (its types and possible expressions) this implementation of forward mode operates on. The file includes some "syntactic sugar" to make the concrete syntax examples of our language easier to read. The algebra is defined in `Algebra.hs`. The `PrettyPrint.hs` module contains example of an algebra instance of the language and its types, namely a pretty printer.

## Preprocessing
`Annotate.hs` fills in all empty BinOp types.
`Variable.hs` renames all variables to a unique one. 

`StaticCheck.hs` can be used for type-checking.

## Differentiation
The differentiation algorithm is a simple fold defined in `Forward.hs`.

Numerical differentiation (for validation tests) is implemented in `Numerical.hs`. 

## Evaluation
An `evaluate` function is given both in `FoldMachine.hs` and `StackMachine.hs`. 



