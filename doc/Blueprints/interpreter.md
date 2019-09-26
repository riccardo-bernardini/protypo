# Internal representation
We use a tree-based internal representation.  Every node has an "action" and sub-trees as parameters.  For example, WHILE has two sub-tree: the condition and the loop body. 

Node actions
* Structured statements
   * IF  (expression, then, else)
   * FOR  (index, iterator, body)
   * WHILE (condition, body)
   * LOOP (body)
   * statement list
* Simple statements
   * Procedure call
   * assignment (name list, expression list)
* Exit statements
   * EXIT (loop label)
   * RETURN (list of expression)
* Expressions
   * Math operators
   * DOT (symbol reference, identifier)
   * Function CALL (reference, parameter list)

# Internal values

Internally we have a type `Engine_Value` that represents all the possible internal types.  The internal types are

* `Void` represents "no value," maybe useful in some contexts.
* Scalar types: `Integer`, `Float`, `String` (Characters are just strings of length 1.  Do we need a special type?)
* Record-like types: they are represented by lib-user-defined types that implements a `Selectable` interface
* Function-like types: they can be queried with a parameter list.  There are three subtypes
   * Arrays and builtin functions represented by lib-user-defined types that implement interface `Indexable`
   * template-defined functions that are represented by a tree
* Iterators, used in `for` loops, represented by types implementing the `Iterator` interface

Functions and field selects return values of type `Engine_Value`

# Execution model 

It is just a visit of the tree in depth-first order, but possibly with "lazy" evaluation.  For example, the execution of a conditional branch first evaluates the  condition, then decides which branch to follow.

## Symbol table
At every time there is an active symbol table, a new level is opened
* In a `for` loop for the index variable (the new level is over the current environment)
* In the call of a template-defined function (the new level is a children of the root table)

## Returned values
The execution of some action nodes can return values.  In particular,
* Math operations
* Function calls (more than one)
* DOT and indexes
All the other statement return no value

## Breaking out
There are two "breaking out" instructions
* BREAK
* RETURN
The way to process these instructions is as follows
* Mark in the interpreter status that we encountered a breaking instruction (and which type)
* Record in the interpreter status the optional associated value (loop label for BREAK, return list for RETURN)
* Return to the caller

The *breaking flag* is handled as follows
* Inside the function for executing a list of commands, every time a command returns the state of the interpreter is checked for breaking, if a breaking instruction was encountered, the function returns.
* Inside the portion that handle a loop
    * If the break was an EXIT AND (no label given OR the label is the label of the loop), THEN the breaking flag is reset. The loop is interrupted in any case.  
* Inside the portion that handles a function call
    * If the break was a RETURN, get the result list, reset the flag and return to the caller
    * If it was a EXIT, this is an error
* If the interpreter returns to the top-level call in a breaking state it is an error

## Different evaluation types

There are at least three different types of "evaluation" of trees
* Pure statements (including composite ones). No value is returned
* Expression evaluation.  A list of scalar may be returned (because of RETURN).  A scalar is an integer, a float or a string.
* Name evaluation.  This gives a "pointer" to the symbol table that be queried for values and/or writing it.

More precisely, a name can evaluate to
* An array handler that exports a method that accepts a list of parameters and returns an `Engine_Value`
* A record handler that exports a method that accepts an ID  returns an `Engine_Value`
* A function handler that exports a callback method
* A simple handler that allows to read/write the value

Every handler has both versions read-only and read-write.

The evaluation of a name (the result of a concatenation of dot and indexing operator) works as follows
* The DOT operator accepts as parameters a name and an ID.  The name must evaluate to a record handler.  The record handler is queried with the ID and the result must be a handler of the above
* The INDEX operator accepts as left parameter a name (that must evaluate to a record or a function handler) and a list of expressions.  Every expression is evaluated and the callback of the handler called.  If the handler is an array handler, it accepts a fixed number of arguments, without any optional parameter handling, in the case of functions optional parameters are allowed. In the case of the record the outcome is a handler, for a function it is a scalar.
* An identifier evaluates to its entry in the symbol table. If the stored value is a scalar, a simple handler is wrapped around it.

For example, suppose the following name is evaluated
```
foo.bar(2).zip
```
* First `foo` is searched in the ST.  The value stored is a record handler that is the result of the evaluation of `foo`
* Next, the handler above is queried with the string `bar`.  The query returns an array handler.
* The returned array handler is queried with the expression list `(2)`, the result is a record handler
* Finally, the record handler is queried with `zip`.  The final result is a simple handler

Now, suppose the name above is used as
```
print(foo.bar(2).zip);
foo.bar(2).zip := 42;
```
In the first case the simple handler obtained by evaluating the name `foo.bar(2).zip` is queried for a value; in the second case the handler is used to write a value in `foo.bar(2).zip` (assuming that the simple handler is read-write).
   

    
