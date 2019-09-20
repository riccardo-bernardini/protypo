# Internal details 

## Abstract
This file is a collection of notes about the internal details of the templating engine

## Front-end scanner state machine
This is the state diagram of the front-end scanner

![](state-diagram.svg)

The front-end scanner gets in input a text and produces a sequence of tokens. Every transition not esplicitely shown in the diagram, remains in the same state. End-of-file is illegal while in Code state, in every other state it causes a transition to Doc and the end of scanning.

The output of the front-end scanner is given to the parser.

From the diagram one can see that the Doc state acts like a "central state" where every other state returns.  This suggest to implement the other states as "sub-scanners" called by the central one.

## Internal values
Values are kept internally as values of type Engine_Type.
