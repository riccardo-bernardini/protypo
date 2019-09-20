# Protypo
Another template engine "alla mia maniera" (in my style).

## Abstract

This library is a templating engine that I wrote because unsatisfied of what I found around. It is though to be quite flexible and with a simple conceptual model.  The language syntax is very Ada-like. At the moment it is not strongly typed, but rather duck-typed "a la Ruby."

## The model
A template is a program.  Every statement in the program produces an output (that can be void) of type Engine_Value  that is passed to a "consumer" defined by the library user (that is, who writes the application that uses this library).  The consumer can do whatever it wants with the output; in the default case the statement output is converted to string (when meningful) 

A special characteristic of the template language is that any expression can be used as statement (a la C).  A basic syntax can be

```
statement := simple_statement | compound
simple_statement := assignment | return | string | name
compound  := if | loop | case
```

Syntax is very Ada-like, but with some semplification (e.g, no task, type definitions, ...), allowing for indexing components and selectors.  Also assignement could accept a list of names on the LHS.  The only statements that return non-void values are "naked expressions" like `"foo";` or `42;`, any other statement (including assignments) return void.

## "Here doc" syntax

The template syntax is indeed a bit peculiar, with a kind of "here doc" implicit that is transformed in normal code by a pre-processor. 

More precisely, the parser has two states: doc and code.  Initially the state is "doc."  When in the doc state the parser reades the characters one at time and collect them in a string.  The state changes to code when the marker `#{` is found.  On the "doc-> code" transition the string is closed and output by the pre-processor as a normal string.  When the processor is in the code state it just copies the text from input to output.  It goes back to doc when the marker `}#` is found.  At the end of the file the state must be doc.
A special case is when the code is represented by a simple name.  In this case the syntax is a # directly followed (without spaces) by the name itself, for example #bar.field, we return back to code at the end of the name. This construction is equivalent to the `#{...}#` construction, for example, `#bar.field` is equivalent `#{ bar.field; #}` 

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

## Comments 

We distinguish between three types of comments
* In-code comments
* Template comments
* Target comments and *transparent* target comments

The first type of comment is found in the code sections within `#{...}#`, they begin with -- and end at the end of line. 

Template comments are found in the doc section, begin with `#--` at the beginning of the line and end the end of the line.  Template comments are just ignored, their presence has no influence on the result.

Target comments are still found in the doc section. They are considered comments for the target language and are copied as they are in the output string. Inside target comments **start-of-code markers and marked names are ignored**.  If we want to be able to generate part of the target comment with code, we can write a *transparent* target comment that is a special case of target comment.  The marker for transparent target comments can be replaced by another string before storing it in the output string.

The markers for template and target comments are configurable to be adapted to the specific target language.

### Example

In case of LaTeX as target language, transparent target comments begin with '%#\s' and it is replaced by '%'.  Therefore,

```
% This is just a comment #{ and this is ignored }# #this.too
%# This is a transparent comment and #this.is.not.ignored (comment again)
```
is mapped to

```
"% This is just a comment #{ and this is ignored }# #this.too
% This is a transparent comment and "; this.is.not.ignored; "(comment again)";
```

## Types

The template engine has the following types
* Scalars: integer, float and strings (maybe there is no need for enum and characters)
* Record 
* Arrays
* Maps (?)
* Iterators
* Void, representing a "non-value"
