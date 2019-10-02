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
Consider the following simple example of template
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

As you can see, there are two different type of sections
1. *Free-text sections* (e.g., "This is the header", "this is the #k#-th line of the body")
2. *Code sections* between `#{...}#`

The idea is that free-text sections are copied *as-they-are* to the output, while code sections are executed. 
The interaction between code and free-text sections can be a bit confusing at first. The best way to understand it is to explain what the library does internally.  When the template is read the first stage transforms the free-text sections in code that sends the section content to the output.  For example, the template above is changed internally to something equivalent to
```
#{
@("This is the header");

for k in range(1, 5) loop
   @("this is the #k#-th line of the body");
end loop;

@("This is the trailer");
}#
```
where `@` is a builtin-function that sends its parameter to the output.  I

> f you are crincing to the name choice, just know that most probably you will never need to use it explicitely (although you can, it is a normal function) and I wanted something unlikely to cause clashes. 

Most probably you noticed the expression `#k#` in the free-text and you probably already guessed that they will be replaced by the corresponding value when the text is sent to the output.  If you are familiar with Ruby, this is similar to the `#{...}` mechanism inside `"`-strings. 
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

#### A more complex example

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

#{ 
    for user in participants loop 
       [ \hline #user.last_name#  #user.first_name# & #user.institution# & \\ ]
    end loop; 
}#

   \hline
   \end{tabular}
\end{center}
\end{document}
```


#### Syntax overview
It is worth giving a brief overview of the syntax, a more complete description can be found in the `doc/` folder.

As said above, syntax is very Ada-like. Again, maybe the best thing is an example.  Let's start with function definition 
```
#{
  function factorial(x) -- Procedures are possible, too.  Oh, BTW, this is a comment
  begin 
     if x < 1 then
        return 1;
     else 
        return x*factorial(x-1);
     end if;
  end factorial; 
  
  function fibonacci(n ; x2:= 1; x1:=1) -- Optional parameters too?  Wow!
  begin
    old := x2;
    current := x1;
    counter := 1;
    
    while counter < n loop
       tmp := current + old;
       old := current;
       current := tmp;
       n := n+1;
    end loop;
    
    return current;
  end fibonacci;
  
  function mod_div(num, den) 
  begin
    quotient := num / den; -- I'm supposing they are integers
    rem      := num - quotient * den;
    
    return quotient, rem; -- Functions can return multiple values
  end mod_div;
  
  q, r := mod_div(13, 5);  -- Multiple assignment
  
  x, y := y, x;  -- Old trick 
}#
```
Not bad for a template language, right? (Yeah, let me brag a bit  ;-) 

If you program in Ada I guess you'll feel at home.  Two major differences that maybe you noticed is the absence of type declaration (to keep things simple, it is for templates after all...) and the fact that functions can return more than one value.  The latter feature was added to turn around the fact that there are no `out` parameters.  If you allows for multiple return values, you must allow multiple assignment too.

Let's talk about loops.  You saw that we have `while` loops, we have `for` and never ending `loop`s too

```
#{
  foo : loop 
      for user in subscriptions loop
         if user.name /= "boo boo sette-te" then
            [ \newuser{#user.name#}{#user.age#} ]
         else
            exit foo;
         end if
      end loop;
  end loop;
}#
```
As you can see we have `exit` like in Ada (with or without the loop label).  There is not `exit when` construction (still for simplicity). `continue` is not implemented yet, but I  plan to add it.

It is worth spending a couple of words on `for` loops
```
for user in subscriptions loop
   [name = #user.name#]         
end loop;
```
As you can see, they are a generalized version of the classical `for` and based on `iterator`s. This allows you to iterate over the element of a structure, e.g., the users that subscribed to something.  Maybe you are wondering how you can define an iterator. You cannot define it with the template language, usually it is the application using the template engine that defines variables holding iterators that use callbacks inside the application to implement the iterator.  More about this in the "programmer tutorial" section.

If you want the classical `for` you can use the built-in function `range` that returns an iterator
```
for k in range(1, 6) loop
   [#k#-th iteration]
end loop;
```
Yes, no special `..` syntax. 

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
