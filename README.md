# Protypo
Another template engine "alla mia maniera" (in my style)

## The model
A template is a program.  Every command/structure in the program produces an output string (that can be empty) and the evaluation of the whole program is the concatenation of the produced strings.

A special characteristic of the template language is that even naked strings are commands whose evaluation is the string itself.  A basic syntax can be

```
statement := simple_statement | compound
simple_statement := assignment | return | string | name
compound  := if | loop | case
```

Syntax is very Ada-like, but with some semplification (e.g, no task, type definitions, ...), allowing for indexing components and selectors.  Also assignement could accept a list of names on the LHS.

The evaluation of the statements give the following results
* A string evaluates to itself
* A function evaluates to the returned value, empty string if the return value is void
* A name (LHS of an assignment) evaluates to its value, converted to string
* An if or a case evaluates to the evaluation of the selected branch
* A loop evaluates to the concatenation of the evaluations of every iteration of its body
* Any definition evaluates to the empty string

## Special format

The template syntax is indeed a bit peculiar, with a kind of "here doc" implicit that is transformed in normal code by a pre-processor. 
More precisely, the parser has two states: doc and code.  Initially the state is "doc."  When in the doc state the parser reades the characters one at time and collect them in a string.  The state changes to code when the marker '#{' is found.  On the "doc-> code" transition the string is closed and output by the pre-processor as a normal string.  When the processor is in the code state it just copies the text from input to output.  It goes back to doc when the marker }# is found.  At the end of the file the state must be doc.
A special case is when the code is represented by a simple name.  In this case the syntax is a # directly followed (without spaces) by the name itself, for example #bar.field, we return back to code at the end of the name. This construction is equivalent to the #{...}# construction, for example, #bar.field is equivalent #{ bar.field; #} 

### Example
For example, the following 
```
\section{Foo}

\begin{wp}
#{ for WP in project.wps loop }#
\wpitem{#WP.index}{#WP.name}{#WP.begin}{#WP.end}
#{ end loop; }#
\end{wp}
```

is equivalent to
```
"\section{Foo}

\begin{wp}";
for WP in project.wps loop
   "\wpitem{"; WP.index; "}{"; WP.name; "}{"; WP.begin; "}{"; WP.end; "}"
end loop;
"\end{wp}";
```
