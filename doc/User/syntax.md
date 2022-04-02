# Syntax
This document describes the syntax of the template language.  The description is ABNF-like, even if not strictly formal. Uppercase identifiers are keywords, strings different from meta-characters (*, |, =, []) stand for themselves, metacharacters are prefixed with '

```
program   = sequence_of_statement  EOT
sequence_of_statement = statement*
statement = simple | compound
simple    = assignment | return | proc_call | exit | embedded_text
compound  = if | loop | while | for | defun
defun     = (FUNCTION | PROCEDURE) ID [ '( signature ') ] IS BEGIN sequence_of_statement END ID;
signature = mandatory ; optional | mandatory | optional 
mandatory = ID (; ID)*
optional  = ID := expr (; ID := expr)*

embedded_text = '[ content ']
content       = bracket_free | bracket_free? '[ content '] bracket_free?
bracket_free  = [^[]]+

proc_call  = ID [ '( expr_list ') ]  ;
assignment = name_list := expr_list ;
name_list  = name (, name)*
name       = ID | name . ID | name '( expr_list ')
return     = RETURN [ expr_list ] ;

expression = relation  [(AND relation)+ | (OR relation)+ | (XOR relation)+]
relation   = simple_exp [ COMP simple_exp ]
simple_exp = term [(+|-) simple_exp]
term       = factor [('*|/) term]
factor     = [ unary_op ] primary
unary_op   = +|-|NOT
primary    = '( expression ') | name | NUMBER | TEXT | CAPTURE '( proc_call ')
expr_list  = expression [, expr_list]

if         = IF expression THEN sequence_of_statement 
             (ELSIF expression THEN sequence_of_statement)* 
             [ELSE sequence_of_statement] 
             END IF ;
             
loop       = [ ID : ] LOOP sequence_of_statement END LOOP;
for        = [ ID : ] FOR ID IN expression loop
while      = [ ID : ] WHILE expression loop

exit       = EXIT [ ID ] ;
```

