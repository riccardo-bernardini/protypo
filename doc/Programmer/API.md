# Index
* [Summary](#summary)
* [An example](#an-example)
* [The API](#the-api)
   * [The interpreter](#the-interpreter)
     * [Low level interface](#low-level-interface)
   * [The `Engine_Value` type](#the-engine_value-type)
     * [Array handlers](#array_handler)
     * [Record handlers](#record_handler)
     * [Ambivalent handlers](#ambivalent_handler)
     * [Constant and reference handlers](#constant_handler-and-reference_handler)
     * [Iterator handlers](#iterator_handler)
     * [Function handlers](#function_handler)
       * [Callbacks](#syntactic-sugar-using-callbacks)
   * [The wrappers](#wrappers)
     * [Constant wrapper](#constant-wrapper)
     * [List wrapper](#list-wrapper)
     * [Range iterator](#range-iterator)
     * [Record wrappers](#record-wrappers)
       * [Map Based](#map-based)
       * [Enumeration Based](#map-based)
     * [Array wrapper](#array-wrapper)
       * [Basic array wrapper](#basic-array-wrapper)
       * [Generic array wrapper](#generic-array-wrapper)

# Summary

This document describes, to a fair level of detail, the main features of the API available to the programmer that wants to use the library in an application. The finer details are omitted, but they are easily deduced by the _specs_ files under `Protypo.API` (all the API resources are descendants of `Protypo.API`).

Our _battleplan_ is as follows
1. First we give a "taste" of the API by means of [a tutorial example](#an-example)
1. Successively we enter into the details of the API that can be partitioned into three wide areas
   1. [The interpreter](#the-interpreter), that is, the object that interprets the template and carries out the expansion
   1. [The `Engine_Value` type](#the-engine_value-type) that represents the "data" that the interpreter processes. An `Engine_Value` can represents several types of data (Integer, Float, arrays, records, ...) that will be described in the detail
   1. [The builtin wrappers](#wrappers).  They are structure that ease the duty of converting application data (expressed in Ada type) into `Engine_Value`s suitable for the interpreter.


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
> Builtin function `range` and fields `range` and `iterate` of an _array wrapper_ return iterators.

### `Function_Handler`

_Function handlers_ are used to implement programmer-defined functions. Their interfaces is a bit complex
```Ada
   -- Definition of parameter signature type to describe the interface of 
   -- programmer-defined functions
   type Parameter_Class is (Mandatory, Optional, Varargin);

   type Parameter_Spec (Class : Parameter_Class := Mandatory) is
      record
         case Class is
            when Mandatory | Varargin =>
               null;

            when Optional =>
               Default : Engine_Value;
         end case;
      end record;

   type Parameter_Signature is array (Positive range <>) of Parameter_Spec;

   function Is_Valid_Parameter_Signature (Signature : Parameter_Signature) return Boolean;
   --
   -- Return True if Signature is a valid parameter signature that can be returned
   -- by Signature method.  A valid signature satisfies the following "regexp"
   --
   --   Void_Value* Non_Void_Value* Varargin_Value?
   --
   -- that is,
   -- * there is a "head" (potentially empty) of void values that
   -- mark the parameters that are mandatory and have no default;
   --
   -- * a (maybe empty) sequence of non void values follows, these are
   -- default values of optional parameters
   --
   -- * the last entry can be Varargin_Value, showing that the
   -- last parameter is an array (maybe empty) that collects all the
   -- remaining parameters
   --
  
   -- Definition of the function handler interface --
   --------------------------------------------------

   type Function_Interface is interface;
   type Function_Interface_Access is access all Function_Interface'Class;

   function Process (Fun       : Function_Interface;
                     Parameter : Engine_Value_Array)
                     return Engine_Value_Array is abstract;


   function Signature (Fun : Function_Interface)
                                return Parameter_Signature is abstract
     with Post'Class => Is_Valid_Parameter_Signature (Signature'Result);
```
Neglecting for a moment the `Parameter_Signature` type, the two functions that a function handler must provide  are
```Ada 
   function Process (Fun       : Function_Interface;
                     Parameter : Engine_Value_Array)
                     return Engine_Value_Array is abstract;


   function Signature (Fun : Function_Interface)
                                return Parameter_Signature is abstract
     with Post'Class => Is_Valid_Parameter_Signature (Signature'Result);

```
Function `Process` accepts its parameters as an `Engine_Value_Array` and returns its result in an `Engine_Value_Array` (that can be empty if the function is actually a procedure, the constant `No_Value` is convenient way to return an empty result). Function `Signature` instead returns a `Parameter_Signature` that describes which parameters are mandatory, which optionals and if an arbitrary number of parameters are accepted.

Let's examine more closely the type `Parameter_Signature`
```Ada
  type Parameter_Class is (Mandatory, Optional, Varargin);

   type Parameter_Spec (Class : Parameter_Class := Mandatory) is
      record
         case Class is
            when Mandatory | Varargin =>
               null;

            when Optional =>
               Default : Engine_Value;
         end case;
      end record;

   type Parameter_Signature is array (Positive range <>) of Parameter_Spec;
```
A `Parameter_Signature` is just an array of `Parameter_Spec`. There are three classes of `Parameter_Spec`s: `Mandatory` parameters, `Optional` parameters (with a default value) and `Varargin`. The latter spec is for the parameter (necessarily the last one) that "absorbs" the tail of remaining parameters.
> In case you are curious, `Varargin` comes from Matlab that uses this auto-magical parameter to handle an arbitrary number of parameters.

> Currently `Varargin` is not fully implemented, but it will be...

Of course, in order for a `Parameter_Signature` to make sense the mandatory parameters must precede the optional ones and the `Varargin` parameter (if present) must necesseraly be the last one. The validity of a `Parameter_Spec` is checked by function 
```Ada
   function Is_Valid_Parameter_Signature (Signature : Parameter_Signature) return Boolean;
```
When the interpret finds a function call, first it collects the parameters of the call, successively calls `Signature` and it uses the result to handle default parameters and, possibly, the `Varargin`. Finally, it calls the function `Process`.

> You maybe notice that `Process` does not return a single `Engine_Value`, but an array of `Engine_Value`. Since no mechanism is given for `out` (or `in out`) parameter modes, we compensate using the Matlab solution: a function can return more than one value.  Therefeore, `Process` must return an array.

#### Syntactic sugar: using callbacks

In order to simplify the definition of programmer-defined function a "shortcut" (based on the function handler above) is provided.
```Ada 
   type Callback_Function_Access is
      not null access function (Parameters : Engine_Value_Array) return Engine_Value_Array;

   function Create (Val          : Callback_Function_Access;
                    N_Parameters : Natural := 1) return Engine_Value
     with Post => Create'Result.Class = Function_Handler;

   function Create (Val            : Callback_Function_Access;
                    Min_Parameters : Natural;
                    Max_Parameters : Natural;
                    With_Varargin  : Boolean := False) return Engine_Value
   
```
A callback function is just a function with the same interface as `Process`; the `Create` function takes care of "wrapping" it in a suitable function handler. The second form defines a function that has `Min_Parameters` mandatory parameters, `Max_Parameters-Min_Parameters` optional parameters (whose default value is `Void_Value`) and the last argument is a _varargin_ if `Varargin` is true.

In order to ease the processing of callback parameters the following type and functions are provided
```Ada 
   type Parameter_List is private;
   -- This is type that can help writing callbacks.  A parameter list
   -- is created using the Engine_Value_Array given to the callback
   -- and it is possible to read its element one at time, using Shift
   -- (that removes the first element too, kind of shift in Ruby, bash, ...)
   -- or Peek (that does not change the list)

   function Create (Params : Engine_Value_Array) return Parameter_List
     with Post => Length (Create'Result) = Params'Length;

   function Length (List : Parameter_List) return Natural;

   function Is_Empty (List : Parameter_List) return Boolean
   is (Length (List) = 0);

   function Shift (List : in out Parameter_List) return Engine_Value
     with Post => (if Is_Empty (List'Old)
                       then Shift'Result = Void_Value
                         else Length (List) = Length (List'Old)-1);
   -- Return the first parameter and remove it from the list.  If the
   -- list is empty, return Void_Value

   function Peek (List : Parameter_List) return Engine_Value
     with Post => (if Is_Empty (List) then Peek'Result = Void_Value);
   -- Return the first parameter
```
##### Example of callback-defined function
This is an example of how to define a function that accepts two parameters and returns quotient and remainder of the integer division. The second parameter is 2 by default. This is how the callback is defined

```Ada
   function Div_Mod
     (Params : Protypo.Api.Engine_Values.Engine_Value_Array)
      return Protypo.Api.Engine_Values.Engine_Value_Array
   is
      use Protypo.Api.Engine_Values;

      Parameters : Parameter_List := Create (Params);

      X : constant Integer := Get_Integer (Shift (Parameters));
      Y : constant Integer := Get_Integer (Shift (Parameters), 2); -- Default handling

      Result : Engine_Value_Array(1..2);

   begin
      Result (1) := Create (X / Y);
      Result (2) := Create (X mod Y);
      return Result;
   end Div_Mod;

```
This is how it is included in the interpreter (we suppose `Engine` is declared as `Engine : Interpreter_Type`).
```Ada
   Engine.Define (Name  => "divmod",
                  Value => Create (Val            => Div_Mod'Access,
                                   Min_Parameters => 1,
                                   Max_Parameters => 2));
```

## Wrappers 

The mechanism provided by the handlers is very flexible, but creating a handler can be slightly bothersome. Since  there are few types that are typically used, the library provides few _wrappers_ that make easier to export application variables to the template. Currently the available wrappers are
* [Constant wrapper](#constant-wrapper)
* [List wrapper](#list-wrapper)
* [Range iterator](#range-iterator)
* [Record wrappers](#record-wrappers)
* [Array wrapper](#array-wrapper)

### Constant wrapper
This is maybe the simplest wrapper. The interface is as follows
```Ada
   type Constant_Wrapper is new Constant_Interface with private;
   type Constant_Wrapper_Access is access Constant_Wrapper;

   function Read (X : Constant_Wrapper) return Engine_Value;

   function Make_Wrapper (Value : Engine_Value) return Constant_Wrapper_Access;
   function Make_Wrapper (Value : Integer) return Constant_Wrapper_Access;
   function Make_Wrapper (Value : String) return Constant_Wrapper_Access;
   
   function To_Handler_Value (Value : Engine_Value) return Handler_Value;
   -- Equivalent to Create(Make_Wrapper(Value))
   
   function To_Handler_Value (Value : Integer) return Handler_Value;
   -- Equivalent to Create(Make_Wrapper(Value))
   
   function To_Handler_Value (Value : String) return Handler_Value;
   -- Equivalent to Create(Make_Wrapper(Value))
   
```
Besides defining the `Read` function (as required by `Constant_Interface`) it defines few `Make_Wrapper` functions that create a `Constant_Wrapper` with the specified value. The functions `To_Handler_Value` are just syntactic sugar for the concatenation of `Make_Wrapper` with `Create`.

### List wrapper
```Ada
   type List is tagged private;

   procedure Append (Item  : in out List;
                     Value : Engine_Value);

   function Iterator (Item : List) return Iterator_Interface_Access;
```
This also is very simple: it allows to construct a list of `Engine_Value` (by appending new values to the list) and it exports an iterator that iterates on the list.

### Range iterator
```Ada
   type Range_Iterator is new Iterator_Interface with private;

   procedure Reset (Iter : in out Range_Iterator);
   procedure Next (Iter : in out Range_Iterator);

   function End_Of_Iteration (Iter : Range_Iterator) return Boolean;

   function Element (Iter : Range_Iterator) return Handler_Value;

   function Create (Start, Stop : Integer) return Iterator_Interface_Access;
```
This is an iterator that runs over the interval of integers specified to the function `Create`. 

### Record wrappers

Two different types of record wrappers are provided: _map based_ and _enumeration based_.

#### Map based 

This kind of wrapper implements a record handler by using a map from string to `Engine_Value`. The interface is as follows

```Ada
package Protypo.Api.Engine_Values.Record_Wrappers is
   package Record_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => ID,
        Element_Type => Engine_Value,
        "<"          => Ada.Strings.Less_Case_Insensitive);

   subtype Record_Map is Record_Maps.Map;


   
   type Record_Wrapper is new Record_Interface with private;
   type Record_Wrapper_Access is access Record_Wrapper;

   function Create_Wrapper return Record_Wrapper_Access;

   
   
   type Record_Map_Reference (Ref : access Record_Map) is limited private
     with Implicit_Dereference => Ref;

   function Map (Item : in out Record_Wrapper) return Record_Map_Reference;


   
   overriding function Get (X     : Record_Wrapper;
                            Field : ID)
                            return Handler_Value;

   overriding function Is_Field (X     : Record_Wrapper;
                                 Field : ID)
                                 return Boolean;
private

  -- Ehi, do not peek! :-)

end Protypo.Api.Engine_Values.Record_Wrappers;
```
Function
```Ada
function Create_Wrapper return Record_Wrapper_Access;
```
creates a new `Record_Wrapper` that has an internal map of type `Record_Map`. The internal map can be manipolated by means of function
```Ada

   function Map (Item : in out Record_Wrapper) return Record_Map_Reference;
```
that returns a reference to the internal map.  This kind of record is very "dynamic" and it can add or remove fields at run-time.

#### Enumeration based

In an _enumeration based record wrapper_ the names of the fields are given by an enumerated type.  This fixes the field names at compile time, but it allows for a very clean interface. The interface is as follows
```Ada 
generic
   type Field_Name is (<>); -- Enumerated type that gives the name of fields
package Protypo.Api.Engine_Values.Enumerated_Records is
   type Aggregate_Type is array (Field_Name) of Engine_Value;
   -- Type that allows to specify an enumerated record similarly
   -- to an Ada aggregate, for example,
   --
   --     (First_Name => Create ("Pippo"),
   --      Last_Name  => Create ("Recupero"),
   --      Telephone  => Create ("3204365972"))

   Void_Aggregate : constant Aggregate_Type := (others => Void_Value);

   type Multi_Aggregate is array (Positive range <>) of Aggregate_Type;
   -- Array of aggregate type.  It allows to write constant "databases"
   -- of enumerated records

   function To_Array (Db : Multi_Aggregate) return Engine_Value_Array
     with Post => (for all Item of To_Array'Result => Item.Class = Record_Handler);
   -- Convert an array of aggregates into an Engine_Value_Array whose
   -- entries are Enumerated_Records.
   -- Very useful in initializing other wrappers (e.g., from Array_Wrappers)


   type Enumerated_Record is new Record_Interface with private;
   type Enumerated_Record_Access is access Enumerated_Record;


   function Make_Record (Init : Aggregate_Type := Void_Aggregate)
                         return Enumerated_Record_Access;

   procedure Fill (Item   : in out Enumerated_Record;
                   Values : Aggregate_Type);

   procedure Set (Item  : in out Enumerated_Record;
                  Field : Field_Name;
                  Value : Engine_Value);

   function Get (Item  : Enumerated_Record;
                 Field : ID)
                 return Handler_Value;

   function Is_Field (Item : Enumerated_Record; Field : ID) return Boolean;
private
  -- 
end Protypo.Api.Engine_Values.Enumerated_Records;

```
An `Enumerated_Record` is created with the function of 
```Ada
function Make_Record (Init : Aggregate_Type := Void_Aggregate)
                         return Enumerated_Record_Access;
```
that accepts an initial value of type `Aggregate_Type` which is simply an array of `Engine_Value` indexed by the field names
```Ada
 type Aggregate_Type is array (Field_Name) of Engine_Value;
```
This allows for a very Ada-like notation. Supposing one has a user record with name, surname and phone and that the corresponding package (instantiation of the generic `Protypo.Api.Engine_Values.Enumerated_Records`) is `User_Records`, one can write
```Ada
    User : User_Records.Enumerated_Record := User_Records.Make_Record((Name    => "John",
                                                                       Surname => "Smith",
                                                                       Phone   => "123456789"));
```
Often records are collected in lists or arrays. In order to use them in the engine it is necessary to convert them to a `Engine_Value_Array`. For this case the package provides the function
```Ada
   type Multi_Aggregate is array (Positive range <>) of Aggregate_Type;
   -- Array of aggregate type.  It allows to write constant "databases"
   -- of enumerated records

   function To_Array (Db : Multi_Aggregate) return Engine_Value_Array
     with Post => (for all Item of To_Array'Result => Item.Class = Record_Handler);
``` 
that converts the an array of `Aggregate_Type` in an `Engine_Value_Array` that contains the corresponding `Enumerated_Record`.

### Array wrapper 

Also for the array structure two wrappers are provided: _basic array wrapper_ and a _generic array wrapper_, the latter is built upon the former. 

> Hmmm.... Maybe I should choose better names

#### Basic array wrapper

#### Generic array wrapper

### Table wrapper

This wrapper implements a two-dimensional table-like structure.  A table is a sequence of _rows_, every row has the same number of columns.  Every column can have a _label_ and a _title_.  The difference between a label and a title is that the label has the syntax of an identifier and it can be used to identify a column, a title is just a free-format string.


