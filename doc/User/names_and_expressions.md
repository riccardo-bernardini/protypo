# Names, references and values 

A *name* is, roughly, something that evaluates to a *reference* to some kind of value, in a sense to a kind of "pointer" to an object. The reference can be both read and written.

From a syntactic point of view we have three kind of names:
1. `ID`
2. `name . ID`
3. `name ( expr )`

* In the first case the ID evaluates to `Variable_Reference` or `Constant_Reference` They represent a "pointer" into the symbol table and can be queried to get the value or asked to update the value.
* In the second case the name is evaluated to get a reference. The reference (that is a kind of pointer) is queried to get the actual value that must be a descendant of `Record_Interface` or `Ambivalent_interface`. The `Get_Field` method is queried to get the new reference.
* In the third case the name is evaluated to get a reference; the reference is queried for the actual value which must be of type `Array_Interface` or `Ambivalent_Interface`. The object is queried with the indexed access method to get a new reference.

If the name appears on a LHS of an assignment, then the final reference is used to write the new value (if the reference is read/write), otherwise it is further queried to get the actual value.
