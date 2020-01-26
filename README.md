# Description
This code implements an automatic differentiation algorithm known as forward mode.

It operates on a small functional language.

Link or cite the paper here. 

# How to do anything?
Yes! We will explain it here!

If you are not satisfied and want to tinker with the code, see the Code Structure section below.

# Code Structure
Write something useful about `Main.hs` and possibly `Examples.hs` here!! 
After that go into detail about the other modules!!

`Language.hs` defines the language (its types and possible expressions) this implementation of forward mode operates on. The file includes some "syntactic sugar" to make the concrete syntax examples of our language easier to read. The algebra is defined in `Algebra.hs`. The `PrettyPrint.hs` module contains example of an algebra instance of the language and its types, namely a pretty printer.

## Preprocessing
`Annotate.hs` fills in all empty BinOp types.
`Variable.hs` renames all variables to a unique one. 

`StaticCheck.hs` can be used for type-checking.

## Differentiation
The differentiation algorithm is a simple fold defined in `Forward.hs`.

Numerical differentiation (for validation tests) is implemented in `Test.hs`. Maybe we should change its name. 

## Evaluation
An `evaluate` function is given in `AbstractMachine.hs`.



