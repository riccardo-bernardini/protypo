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
factor     = '( expression ') | name | NUMBER | TEXT
expr_list  = expression (, expression)*

if         = IF expression THEN sequence_of_statement 
             (ELSIF expression THEN sequence_of_statement)* 
             [ELSE sequence_of_statement] 
             ENDIF ;
```
