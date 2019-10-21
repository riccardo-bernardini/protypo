# User-level tutorial

## Abstract 
This document is aimed to *users* (that is, those who write templates to be expanded) and it introduces the template language by means of examples. Here we aim for a fast and simple introduction to the main features of the language, skipping over many details.  A more detailed description of the language can be found in other documents. 

## A first example

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

> If you are crincing to the name choice, just know that most probably you will never need to use it explicitely (although you can, it is a normal function) and I wanted something unlikely to cause clashes. 

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


## Syntax overview
It is worth giving a brief overview of the syntax, a more complete description can be found in the `doc/` folder.

As said above, syntax is very Ada-like. Again, maybe the best thing is an example.  Let's start with function definition 
```
#{
  function factorial(x) is -- Procedures are possible, too.  Oh, BTW, this is a comment
  begin 
     if x < 1 then
        return 1;
     else 
        return x*factorial(x-1);
     end if;
  end factorial; 
  
  function fibonacci(n ; x2:= 1; x1:=1) is -- Optional parameters too?  Wow!
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
  
  function mod_div(num, den) is
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

In template expansion there is often the necessity of using a  "boilerplate" shared among different templates.  Because of this, we allow for the inclusion of an external file with an `include` *directive*
```
#(include /path/to/file)
```

### How directives are parsed

When scanning the main text a string of the form
```
#(commmand parameter)
```
is a *directive* used to control the parser.  More precisely, a directive is processed as follows
1. First, the whole *directive string* (that includes both `command` and `parameter`) is extracted.  This is done a-la Postscript, taking inside the directive string every matching pair of parenthesis. For example, with
```
#(foo (1) and (2 (a)))
```
the `command` is `foo` and `parameter` is `(1) and (2 (a))`.  If a single parenthesis is required, it can be escaped with `\`, that is
```
#(foo  only one \( parenthesis)
#(foo  only one \) parenthesis)
```
(in general, if a \ is met, the \ is skipped and the following character sent output without interpreting it)

2. The directive string is matched against the following regular expression
```
/^ *([^ ]*) +([^ ].*)$/
```
and the first sub-expression is the `command`, the second sub-expression is the `parameter`. In other words, the directive name is the first substring made of non-space characters, while the parameter is what remains after removing spaces after the directive name.

### Recognized directives

Currently the following directives are recognized.  

* _include_. Its behaviour is similar to C `#include` or Ruby `load` in the sense that it does a "physical" inclusion of the target file.  This because I expect that in many cases `include` will be used to include common boilerplates and such.  Like C `#include` or Ruby `load`, `include` does not check if a file was already included.
* _with_. Its behaviour is similar to Ruby `require` or (more or less) Ada `with`.  It does a "physical" inclusion of the target file, but it will not include the same file twice. This can be useful to load function libraries.

## Comments 

* In the free-text section comments begin with `#--` and end at the end of the line.  
* In the code section comments  begin with `--` and end at the end of the line

## Types

The template engine has the following types
* Scalars: integer, float and strings (maybe there is no need for enum and characters)
* Record 
* Arrays
* Maps (?)
* Iterators
* Void, representing a "non-value"
