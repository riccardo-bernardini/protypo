# Protypo

## What is this?

This is library that provides a template expansion engine in Ada.

I wrote it because I needed it and nothing that I found satisfied me fully. Moreover, I had lots of fun (and work...) writing it ;-)

I like the final result (but I guess you can say I am partial to it). It is quite flexible and with a simple conceptual model and a syntax is very Ada-ish. At the moment it is not strongly typed, but rather duck-typed "a la Ruby."

## A fast tutorial (or maybe two?)

I guess the best way to introduce you to this library is by means of a simple tutorial.  Being this a library there are two different type of users
1. Users that write templates to be expanded by the engine.  We will call these users *final users* or simply *users*
2. Programmers that include the library into a software of theirs. We will call these users *programmers*

It follows that we actually need *two* tutorials: one to explain to the *users* how to write templates and another to explain to the *programmers* how to include the library into a software.  We begin with the first tutorial

### User-level tutorial

The following is an example of template that could be used to generate a LaTeX file producing a table with the participants to an event (a use case quite similar to the one that triggered the development of this library)
```
#-- This line is totally ignored.  A "free text" section that will be copied verbatim to the
#-- output follows.
%%% 
%%% Automatically generated file: every edit will be lost
%%%
\documentclass{article}
\begin{document}
\begin{center}
  \begin{tabular}{|l|l|l|}
  \hline 
  \multicolumn{1}{c}{Name} & \multicolumn{1}{c}{Institution} & \multicolumn{1}{c}{Signature} \\
#-- 
#-- Here the free text section ends.  Between #{ ... }# we place some code that iterates
#-- over the set of participants (its value is determined by the software that uses the engine)
#-- 
#{ 
    for user in participants loop 
}#
#-- 
#-- Back to free text. Since we are inside the loop this text will be repeated for every user
#--
  \hline
  #user.last_name#  #user.first_name# & #user.institution# & \\
#-- 
#-- Let's close the loop
#--
#{ 
    end loop; 
}#
#--
#-- Free text again, the trailer.
#--
\hline
\end{tabular}
\end{center}
\end{document}
```
As you can see, there are two different type of sections
1. *Free-text section*
2. *Code section*
The idea is that free-text sections are copied *as-they-are* to the output, while code sections are executed. 

The interaction between code and free-text sections can be a bit confusing at first. The best way to understand it is to explain what the library does internally.  When the template is read the first stage transforms the free-text sections in code that sends the section content to the output.  For example, the template
```
This is the header
#{
    for k in range(1, 5) loop
}#
this is the #k#-th line of the body
#{
    end loop;
}#
This is the trailer
```
is changed internally to something equivalent to
```
#{
@("This is the header");

for k in range(1, 5) loop
   @("this is the #k#-th line of the body");
end loop;

@("This is the trailer");
}#
```
where `@` is a builtin-function that sends its parameter to the output.  If you are crincing to the name choice, just know that most probably you will never need to use it explicitely (although you can, it is a normal function) and I wanted something unlikely to cause clashes. 

Most probably you noticed expressions like `#user.last_name#` in the free-text.  You probably already guessed that they will be replaced by the corresponding value when the text is sent to the output.  If you are familiar with Ruby, this is similar to the `#{...}` mechanism inside `"`-strings. 
For example, the last template example will generate this output
```
This is the header
this is the 1-th line of the body
this is the 2-th line of the body
this is the 3-th line of the body
this is the 4-th line of the body
this is the 5-th line of the body
This is the trailer
```
(Yes, `1-th` is not the best English ever, nevertheless... Correcting the template in order to take into account this case is left as an exercise to the reader... ;-)

There is a similar short-cut to embed strings into the code section: just include the text between `[...]`.  For example, the last example can be rewritten as 
```
This is the header
#{
    for k in range(1, 5) loop
      [this is the #k#-th line of the body]
    end loop;
}#
This is the trailer
```
Note that there is no `;` after the `]`.  Personally, I find this a more readable alternative when the text to be inserted is short (avoiding lots of `}#...#{` stuff).


As said above, syntax is very Ada-like, but with some semplification (e.g, no task, type definitions, ...), allowing for indexing components and selectors.  Also assignement could accept a list of names on the LHS.  

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
## Directives
When scanning the main text a string of the form
```
#(commmand parameter)
```
is a *directive* used to control the parser.  More precisely, a directive is processed as follows
1. First, the whole *directive string* (that includes both `command` and `parameter`) is extracted.  This is done a-la Postscript, taking inside the directive string every matching pair of parenthesis. For example, with
```
#(foo (1) and (2 (a)))
```
the `command` is `foo` and `parameter` is `(1) and (2 (a))`.  If a single parenthesis is required, an escape Ada-like (i.e., by repeiting the parenthesis twice) is possible, e.g.,
```
#(foo  only one (( parenthesis)
#(foo  only one )) parenthesis)
```
2. The directive string is trimmed by removing initial spaces.  The string until the first space is the command.
3. The `parameter` is what remains, still left trimmed.
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
