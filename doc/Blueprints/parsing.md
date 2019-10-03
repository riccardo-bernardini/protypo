# Note about parsing

This documents at the moment is just a "colelctor" of notes about parsing.  Do not pay too much attention to it.  In the future it will be cleaned or removed.

Note that a name in basic can denote both a variable access or a function call (they are undistinguishable at the syntax level). By analyzing the FIRST relation we see that
```
proc_call  -> ID
assignment -> name -> ID
return     -> RETURN
if         -> IF
loops      -> ID
etc.
```
Therefore, when we are at the beginning of parsing a `statement`, the next non terminal tells us what kind of statement we are going to parse.
