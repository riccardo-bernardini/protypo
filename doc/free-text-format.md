# The format of the free text section

## Extracting the free text

The engine is parameterized with the following strings
* An escape character: `#` Currently is hard-wired in the code, maybe in a future will be parameterizable
* A *start code* marker: escape character + `{` 
* An *end code* marker: made by `}` + escape
* A *begin directive* marker:  escape + `"`

The way the free text is handled is as follows:

* If the current character
    * is not the escape character, it is added to the free-text
    * is the escape and it is followed by `{`, we scan the code section until we find the end marker.  Note that the end marker is expected to be in places where a token is expected.  For example if it is inside a string (e.g. `"foo }#"`) or an embedded text (e.g. `[{foo}#k#]`) it is not considered as an end marker.
    * is the escape and it is followed by `"` we scan the string until the closing `"` using the Ada-like quoting (i.e., `""` to insert a `"` in the string)
    * is the escape and it is followed by any other character, we put **both** to the free-text

## Free text processing by `@`

When the free text is given to `@` the following processing takes place

* The text is scanned char-by-char
* If the current char 
    * is not the escape character, it is sent as-it-is to the output
    * is the escape character and it is followed by another escape, **both** escapes are discarded and one escape is sent to the output
    * is the escape character **not** followed by another escape, the text is scanned searching for the next escape. The text between the escapes is interpreted as an expression and the result sent to the output

### Example

The following template produces a TeX definition with a variable number of arguments

```
#{
   [\def\foo]
   
   for counter in range(1, nargs) loop
     [###counter#]
   end loop;

   [{Macro content}]
}#
```

How `###k#` is processed?
1. The first `#` is followed by another `#`, therefore both `#` are "eaten" a single `#` is sent to the output
2. The next `#` is followed by `c`, therefore all the text between this escape and the successive (that is, `counter`) is evaluated as an expression.  In this specific case it gives the iteration number.

Summarizing, the template above (say, for `nargs=4`) expands to

```
\def\foo#1#2#3#4{Macro Content}
```
