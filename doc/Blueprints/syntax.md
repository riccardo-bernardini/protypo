# Syntax
This document describes the syntax of the template language.  The description is ABNF-like, even if not strictly formal. Uppercase identifiers are keywords, strings different from meta-characters (*, |, =, []) stand for themselves, metacharacters are prefixed with '

```
program   = sequence_of_statement
sequence_of_statement = statement*
statement = simple | compound
simple    = naked_expr | assignment | return  
compound  = if | case | loop | while | for 

naked_expr = [ expr_list ]
assignment = name_list := expression ;
name_list  = name (, name)*
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
naked_expr -> [                              
assignment -> name -> ID
return     -> RETURN
if         -> IF
etc.
```
Therefore, when we are at the beginning of parsing a `statement`, the next non terminal tells us what kind of statement we are going to parse.  The only ambiguity is when the next terminal is an ID that can begin both a `naked_expr` and an `assignment`.  Since if the first non-terminal is an ID, then what follows is a `name`, we can parse a `name` and the check what follows.

# Semantic 
We translate to a tree-based.  Every node has an "action" and sub-trees as parameters.  For example, IF-THEN-ELSE has three sub-tree: the condition, the THEN branch and (maybe) the ELSE one. 

Node actions
* IF  (expression, then, else)
* FOR  (index, iterator, body)
* WHILE (condition, body)
* Operazioni matematiche
* DOT (symbol reference, identifier)
* CALL (reference, parameter list)
* statement list (vector of nodes)
* naked expression (expression)
* assignment (name reference, expression)
* return (expression optional)
