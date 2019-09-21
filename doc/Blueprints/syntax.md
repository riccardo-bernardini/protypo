# Syntax
This document describes the syntax of the template language.  The description is ABNF-like, even if not strictly formal. Uppercase identifiers are keywords, strings different from meta-characters (*, |, =, []) stand for themselves, metacharacters are prefixed with '

```
program   = sequence_of_statement
sequence_of_statement = statement*
statement = simple | compound
simple    = naked_expr | assignment | return  
compound  = if | case | loop | while | for 

naked_expr = expression ;
assignment = name := expression ;
name       = ID | name . ID | name '( expr_list ')
return     = RETURN [ expression ] ;

expression = term ((+|-) term)*
term       = factor (('*|/) factor)*
factor     = [+|-] basic
basic     = '( expression ') | name | NUMBER | TEXT
expr_list  = expression (, expression)*

if         = IF expression THEN sequence_of_statement 
             (ELSIF expression THEN sequence_of_statement)* 
             [ELSE sequence_of_statement] 
             ENDIF ;
```
Note that a name in basic can denote both a variable access or a function call (they are undistinguishable at the syntax level). By analyzing the FIRST relation we see that
```
naked_expr -> term -> factor -+-> '(, +, -
                              |
                              +-> name -> ID
                              +-> NUMBER, TEXT
                              
assignment -> name -> ID
return     -> RETURN
if         -> IF
etc.
```
Therefore, when we are at the beginning of parsing a `statement`, the next non terminal tells us what kind of statement we are going to parse.  The only ambiguity is when the next terminal is an ID that can begin both a `naked_expr` and an `assignment`.  Since if the first non-terminal is an ID, then what follows is a `name`, we can parse a `name` and the check what follows.

# Semantic 
We translate to a stack based machine. Number, strings and constants are just pushed to the stack.