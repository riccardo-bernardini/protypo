# Internal representation
We use a tree-based internal representation.  Every node has an "action" and sub-trees as parameters.  For example, WHILE has two sub-tree: the condition and the loop body. 

Node actions
* IF  (expression, then, else)
* FOR  (index, iterator, body)
* WHILE (condition, body)
* Operazioni matematiche
* DOT (symbol reference, identifier)
* CALL (reference, parameter list)
* statement list (vector of nodes)
* naked expression (expression)
* assignment (name list, expression list)
* return (expression optional)

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
