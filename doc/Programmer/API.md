# Index
* [An example](#an-example)
* [The API](#the-api)
   * [The interpreter](#the-interpreter)
     * [Low level interface](#low-level-interface)
   * [The `Engine_Value` type](#the-engine_value-type)

# An example

Maybe the fastest way to introduce the basic ideas of the API is by means of an example.

```Ada
with Protypo.Api.Interpreters;
with Protypo.Api.Consumers.File_Writer;
with Protypo.Api.Engine_Values;

with Callbacks;

procedure Simple_Example is
   use Protypo.Api;
   use Protypo.Api.Consumers;

   Engine   : Interpreters.Interpreter_Type;

begin 
   Engine.Define (Name  => "the_answer",
                  Value => Engine_Values.Create (42));

   Engine.Define (Name  => "sin",
                  Value => Engine_Values.Create (Callbacks.Sin'Access));

   Engine.Run (Program  => "sin(1.5)=#sin(1.5)#, 42=#the_answer#",
               Consumer => File_Writer.Open (File_Writer.Standard_Error));
end Simple_Example;
```
Let's analyze the code line by line.  Declaration
```Ada
   Engine   : Interpreters.Interpreter_Type;
```
creates a new *template interpreter*.  With the instructions
```Ada
   Engine.Define (Name  => "the_answer",
                  Value => Engine_Values.Create (42));

   Engine.Define (Name  => "sin",
                  Value => Engine_Values.Create (Callbacks.Sin'Access));
```
we define a variable `the_answer` (whose content is 42) and a function *sin* associated with a callback in `Callbacks`. The values we associate to names with `Define`are of type `Engine_Value` (more about this later) and the functions `Create` convert an "ordinary" value (integer, access to function, ...) to an equivalent `Engine_Value`. 

Finally, we call
```Ada
  Engine.Run (Program  => "sin(1.5)=#sin(1.5)#, 42=#the_answer#",
               Consumer => File_Writer.Open (File_Writer.Standard_Error));
```
the first parameter is the template to be expanded, while the second is the `Consumer` of the output produced by `Engine`. 
A `Consumer` is a type that implements the interface
```Ada
   type Consumer_Interface is limited interface;
   
   procedure Process (Consumer  : in out Consumer_Interface;
                      Parameter : String)
   is abstract;
```
In other words, a `Consumer` is an object that provides a procedure `Process` that expects a `String` parameter. Every time the builtin function `@` is called (directly or, in most cases, indirectly as the result of free-text processing or of `[...]` construction) the parameter of the function, after expansion of `#...#`, is given to the `Process` procedure implemented by the `Consumer`.  Since it is expected that in most cases the output will be sent to a file, package `Protypo.Api.Consumers.File_Writer` provides a pre-defined consumer that writes to file.  In the specific case of the example, it writes to the standard error.

When `simple_example` is run, it prints to the standard error
```console
sin(1.5)= 9.97495E-01, 42= 42
```
# The API

All the resources of interest to the programmer are under the root package `protypo.API`. 
There are two main section of the API that are of interest to the programmer: the *interpreter* that does template expansion and the `Engine_Value` type that represents the values used inside the interpreter. 

## The interpreter

The interpreter interface can be found in the package `Protypo.API.Interpreters`  whose public part contains

```Ada
with Protypo.API.Consumers;
with Protypo.API.Symbols;
with Protypo.Api.Engine_Values;
with Protypo.Api.Consumers.File_Writer;

package Protypo.API.Interpreters is
   type Template_Type is new String;

   type Interpreter_Type is tagged limited  private;

   procedure Define (Interpreter : in out Interpreter_Type;
                     Name        : ID;
                     Value       : Engine_Values.Engine_Value);
   -- Define a new symbol in the global namespace of the interpreter

   procedure Run (Interpreter  : in out Interpreter_Type;
                  Program      : Template_Type;
                  Consumer     : Consumers.Consumer_Access);
   -- Run the specified template and send the result to the consumer

   procedure Expand_Template (Interpreter     : in out Interpreter_Type;
                              Input_Filename  : String;
                              Target_Filenane : String);
   -- Expand the given template and write the result to the specified
   -- target.  To write to standard output use "-"
   
   -- Low-level access --
   ----------------------
   
   type Compiled_Code is limited private;

   
   function Compile (Program  : Template_Type;
                     Base_Dir : String := "") return Compiled_Code;

   procedure Compile (Target   : out Compiled_Code;
                      Program  : Template_Type;
                      Base_Dir : String := "");

   
   procedure Run (Interpreter  : in out Interpreter_Type;
                  Program      : Compiled_Code;
                  Consumer     : Consumers.Consumer_Access);
   -- Run the pre-compiled code and send the result to the consumer


   -- Utilities --
   ---------------
   
   function Slurp (Filename : String) return Template_Type;
   -- Read a template from the specified file.

private
   -- Ehi, it's private stuff! :-)
end Protypo.API.Interpreters;
```
Let's see together the main points.  Type definitions 
```Ada
   type Template_Type is new String;

   type Interpreter_Type is tagged limited  private;
```
define a special string type representing a template to be expanded and the interpreter that will do the expansion.  For covenience we provide the function
```Ada
 function Slurp (Filename : String) return Template_Type;
   -- Read a template from the specified file.
```
that reads a whole file and returns it as a template. 
In order to define a variable/function recognized by the interpreter we can use the procedure
```Ada
   procedure Define (Interpreter : in out Interpreter_Type;
                     Name        : ID;
                     Value       : Engine_Values.Engine_Value);
   -- Define a new symbol in the global namespace of the interpreter
```
that associates the specified `Value` with `Name`. The type of `Name` (`ID`) is defined in `Protypo` and it represents a string that satisfies the usual "identifier constraints" (only letter, digits, underscore and the first character is a letter). 
> The association is done in a _global namespace_, so that they will be visible in every function/procedure defined in the template.

>This association name-value is how we pass data to the template. For example, if we have a template letter that we want to personalize with the recipient name we could define variables `name` and `surname` and write in the template `Dear #name# #surname#,...`
The template can be processed using procedure 
```Ada
   procedure Run (Interpreter  : in out Interpreter_Type;
                  Program      : Template_Type;
                  Consumer     : Consumers.Consumer_Access);
   -- Run the specified template and send the result to the consumer
```
where `Consumer` implements the consumer interface (see above).  Since we expect that the most common case will be reading the template from a file and writing the result to another one, a nice syntactic sugar is provided

```Ada
   procedure Expand_Template (Interpreter     : in out Interpreter_Type;
                              Input_Filename  : String;
                              Target_Filenane : String);
   -- Expand the given template and write the result to the specified
   -- target.  To write to standard output use "-"
```

### Low-level interface

Procedure `Run` does a two-step process: first it compiles the template to an internal form and successively interpret the internal form.  It is possible to do these two steps separately.
> Why?  Well, the real reason is historical: this was the first interface to the interpreter.  It remains here in case it can get useful.

By using 
```Ada
function Compile (Program  : String;
                  Base_Dir : String := "") return Compiled_Code;
```
we get the internally compiled version of the program given as the first argument.  The `Base_Dir` argument is used, for example, when searching for files to be included. By default the current directory of the program is used.

The compiled code can be run using the procedure
```Ada
  procedure Run (Interpreter  : in out Interpreter_Type;
                  Program      : Compiled_Code;
                  Consumer     : Consumers.Consumer_Access);
   -- Run the pre-compiled code and send the result to the consumer
```

## The `Engine_Value` type

All the values manipulated by the interpreter are represented by type `Engine_Value` defined in `Protypo.API.Engine_Values` as  a type with discriminant

```Ada
  type Engine_Value (Class : Engine_Value_Class := Void) is private;
```
where `Engine_Value_Class` represent the specific "type" of the value.  The definition of `Engine_Value_Class` is
```Ada
 type Engine_Value_Class is
     (
      Void,
      Int,
      Real,
      Text,
      Array_Handler,
      Record_Handler,
      Ambivalent_Handler,
      Function_Handler,
      Reference_Handler,
      Constant_Handler,
      Iterator
     );
```
The meaning of the different discriminant is as follows
* `Void` represents... a void value.  Usually used internally and as a placeholder.
* `Int`, `Real` and `Text` are called _scalar values_ and are the equivalent of Ada `Integer`, `Float` and `String`.
* The remaining values `Array_Handler .. Iterator` are used to represents _handler_, kind of callbacks used by the interpreter to access values defined by the application.  They will be described in greater detail later.

The distinction between _handler_, _scalar_ and _numeric_ classes is declared in the Ada code as
```Ada
   subtype Scalar_Classes  is Engine_Value_Class  range Int .. Text;
   subtype Numeric_Classes is Scalar_Classes      range Int .. Real;
   subtype Handler_Classes is Engine_Value_Class  range Array_Handler .. Constant_Handler;

   subtype Handler_Value is Engine_Value
     with Dynamic_Predicate => Handler_Value.Class in Handler_Classes;
```
This allows us to constraints function parameters and return values to belong to a specific class.

In order to create `Engine_Value` with a given value, the package provides many functions `Create` that take a parameter (an integer, a float, an access to a callback object...) and return a properly formed `Engine_Value`.  For example, the `Engine_Value` with the Pi value can be created with
```Ada
Create(3.1415)
```
In order to convert `Engine_Value` back to Ada type the package provides functions like `Get_Integer`, `Get_Float`, `Get_String`, ...

### `Array_Handler`

An _array handler_ is an object that implements the following interface 
```Ada
   type Array_Interface is interface;
   type Array_Interface_Access is access all Array_Interface'Class;

   function Get (X     : Array_Interface;
                 Index : Engine_Value_Array)
                 return Handler_Value
                 is abstract
     with Post'Class => Get'Result.Class in Handler_Classes;


   Out_Of_Range : exception;
```
that is, it must provide a function `Get` that accepts an array of `Engine_Value` and returns an `Engine_Value` **of handler type**, If there is the need of returning a scalar it must be _embedded_ in a [`Constant_Handler`](#costant_handler-and-reference_handler) value. I'll explain the reason for this constraint later.

> Function `Get` is called when the interpreter finds an array expression like `foo(4,5)`. More precisely, when the interpreter process `foo` it looks in the table of defined symbols; if the associate value is an array handler (as we suppose), it collects the expressions between parenthesis (4 and 5 in this case) in an `Engine_Value_Array` and calls `Get`.  Suppose now, for example, that `Get` returns a _record handler_.  If `foo(4,5)` is followed by a selector (e.g., `foo(4,5).name`) the record handler is queried with the field name `name` and so on...

> Why the handler constraint? Because expressions like `foo(4,5)` can be found on the left hand side (LHS) of an assignment. Because of this, even if `foo(4,5)` contains a scalar, we cannot return it directly because it would be useless as LHS. Instead of the scalar we return a [variable handler](#costant_handler-and-reference_handler) that allows both reading and writing of the variable content (think about it like a kind of "address").

### `Record_Handler` 

A _record handler_ implements the following interface
```Ada
   type Record_Interface is interface;
   type Record_Interface_Access is access all Record_Interface'Class;

   function Is_Field (X : Record_Interface; Field : ID) return Boolean
                      is abstract;

   function Get (X     : Record_Interface;
                 Field : ID)
                 return Handler_Value
                 is abstract
     with Post'Class => Get'Result.Class in Handler_Classes;

   Unknown_Field : exception;
```
In other words, a record handler must provide a function `Get` that expects a string with `ID` syntax and return an `Engine_Value` of handler type. The record handler must also export a function `Is_Field` that is used to check if the ID is actually a field.  The same remarks done for [Array_Handler](#array_handler) about the necessity of returning a _handler_ value as result still apply here. See the wrapper [Enumerated_Records](#enumerated_records) for an example of record handler.

### `Ambivalent_Handler`

An _ambivalent handler_ is just something that implements both array and record interfaces
```Ada
  type Ambivalent_Interface is interface
     and Record_Interface
     and Array_Interface;

   type Ambivalent_Interface_Access is  access all Ambivalent_Interface'Class;
```
Strange as it may seem, this is used in the [Array_Wrapper](#array_wrapper) so that one can write (supposing `foo` is an array wrapper) both `foo(5)` and `foo.range` or `foo.first`.  This allows, for example to iterate on `foo` both with a "classical" `for`
```Ada
   for index in foo.range loop
      foo(index) := index+1;
   end loop;
```
or with a more "modern" iterator
```Ada
   sum := 0;
   for item in foo.iterate loop
      sum := sum + item;
   end loop;
```
> The latter example shows that quering an array wrapper with field `iterate` gives an _iterator_ that can be used  to iterate over the array.  To be honest, also `range` returns an iterator, but one that runs over the indexes of the array.

### `Constant_Handler` and `Reference_Handler`

These are two handler that allows to access scalars. A _constant handler_ allows read-only access, a _reference handler_ allows writing also. Their interfaces are as follows
```Ada 
   type Constant_Interface is interface;
   type Constant_Interface_Access  is  access all Constant_Interface'Class;

   function Read (X : Constant_Interface) return Engine_Value is abstract;

   type Reference_Interface is interface and Constant_Interface;
   type Reference_Interface_Access is access all Reference_Interface'Class;

   procedure Write (What  : Reference_Interface;
                    Value : Engine_Value)
   is abstract;
```
Note that actually `Reference_Interface` is a descendant of `Constant_Interface`. See the discussion under [Array_Handler](#array_handler) for the reasons of having these two handlers.

### `Iterator_Handler`

This is used to iterate in `for` loops. Its interface is 
```Ada
   type Iterator_Interface is limited interface;
   type Iterator_Interface_Access is access all Iterator_Interface'Class;


   procedure Reset (Iter : in out Iterator_Interface) is abstract;
   procedure Next (Iter : in out Iterator_Interface) is abstract
     with Pre'Class => not Iter.End_Of_Iteration;

   function End_Of_Iteration (Iter : Iterator_Interface)
                              return Boolean is abstract;

   function Element (Iter : Iterator_Interface)
                     return Handler_Value is abstract
     with Pre'Class => not Iter.End_Of_Iteration;
```
This idea of iterator is inspired by Ada iterators, but with a major difference. In Ada the duty of iterating over the container and the duty of accessing elements are given to separated objects: the former is handled by a discendent of `Forward_Iterator` (or `Reversible_Iterator`) while the latter is assigned to a `Cursor`, with the iterator generating and manipulating the cursor. Here, for the sake of simplicity, everything is handled by the same object.

The interface should be fairly intuitive if you already saw Ada iterator. The meaning of the different subprograms is 
* `Reset` this procedure should reset the iterator to the beginning of the iteration. By calling `Element` right after `Reset` returns the first element of the collection
* `Next` move the next iteration step. After `Next` the value returned by `End_of_Iteration` can become `True`
* `End_Of_Iteration` return `True` if we are beyond the end of iteration. In this state we should not call neither `Next` nor `Element`
* `Element` return the element of the collection that corresponds to the current iteration
