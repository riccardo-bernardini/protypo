# What is this?

This is a package that provides a generic "readable sequence."  A readable sequence is a sequence of elements to be read sequentially, but with some provision to implement backtracks.  They are very convenient in scanner & similar.

# The conceptual model
A *readable sequence* is a sequence of *elements*.  At any time the sequence has a *cursor* that points at an element or beyond the last one (*end of sequence* condition). Procedures are provided to
* Create a new sequence
* Adding element at a sequence (one element at time or using an array)
* Reading the value at the cursor
* Advancing the cursor
* Read and set the current cursor position (to implement backtrack)
* Saving and restoring the cursor position

The package is a generic one, parameterized by the element type and the type of an element array.
