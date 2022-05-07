with Protypo.API.Consumers;
with Protypo.Api.Engine_Values;
with Protypo.Api.Engine_Values.Handlers;

private with Protypo.Symbol_Tables;
private with Protypo.Symbols;
private with Protypo.Code_Trees;

with Utilities;

--
--  This package provides methods to interact with the actual interpreter
--  As in many similar cases, the interpreter action is a two-step
--  process: first the code is compiled into an internal format and
--  successively the internal representation is interpreted.
--
--  This package provides functions and procedures to
--
--  * Compile a template to the internal format
--  * Define variables, functions and procedures for the interpreter
--  * Run a compiled code
--  * Few "syntactic sugar" procedures that, for example, combine both
--    compilation and execution in a single call
--
--  The template code can terminate the execution by using "return" at
--  the top-level.  The values given to "return" are returned as an array
--  of interpreter values to the caller of the procedure Run.
--
--  If the template terminates its execution normally, a vector with
--  a single element equal to True is returned.
--

package Protypo.API.Interpreters is
   type Template_Type is new String;

   type Compiled_Code is limited private;

   procedure Dump (X : Compiled_Code);
   -- Print a tree representation of the content of the compiled
   -- code.  Useful mainly for debug.

   function Compile (Program  : Template_Type;
                     Base_Dir : String := "") return Compiled_Code;

   procedure Compile (Target   : out Compiled_Code;
                      Program  : Template_Type;
                      Base_Dir : String := "");

   type Interpreter_Type is tagged limited  private;

   procedure Define (Interpreter : in out Interpreter_Type;
                     Name        : ID;
                     Value       : Engine_Values.Engine_Value;
                     Read_Only   : Boolean := True);
   -- Define a new symbol in the global namespace of the interpreter

   procedure Define_Function
     (Interpreter : in out Interpreter_Type;
      Name        : Id;
      Funct       : Engine_Values.Handlers.Function_Interface'Class);

   procedure Define_Procedure
     (Interpreter : in out Interpreter_Type;
      Name        : Id;
      Proc        : Engine_Values.Handlers.Procedure_Interface'Class);


   procedure Run (Interpreter  : in out Interpreter_Type;
                  Program      : Template_Type;
                  Consumer     : Consumers.Consumer_Access;
                  Result       : out Engine_Values.Engine_Value_Array);
   --  Run the specified template and send the result to the consumer.
   --  Return the value returned by the script in Result

   procedure Run (Interpreter  : in out Interpreter_Type;
                  Program      : Template_Type;
                  Consumer     : Consumers.Consumer_Access);
   --  Run the specified template and send the result to the consumer.
   --  The value returned by the script is ignored


   procedure Run (Interpreter  : in out Interpreter_Type;
                  Program      : Compiled_Code;
                  Consumer     : Consumers.Consumer_Access;
                  Result       : out Engine_Values.Engine_Value_Array);
   --  Run the pre-compiled code and send the result to the consumer
   --  Return the value returned by the script in Result

   procedure Run (Interpreter  : in out Interpreter_Type;
                  Program      : Compiled_Code;
                  Consumer     : Consumers.Consumer_Access);
   --  Run the pre-compiled code and send the result to the consumer
   --  The value returned by the script is ignored



   Standard_Output : constant String := "-";

   procedure Expand_Template (Interpreter     : in out Interpreter_Type;
                              Input_Filename  : String;
                              Target_Filenane : String;
                              Result          : out Engine_Values.Engine_Value_Array);
   --  Expand the given template and write the result to the specified
   --  target.  To write to standard output use "-" (or the
   --  Standar_Output constant)
   --  The value returned by the script is returned in Result


   procedure Expand_Template (Interpreter     : in out Interpreter_Type;
                              Input_Filename  : String;
                              Target_Filenane : String);
   --  Expand the given template and write the result to the specified
   --  target.  To write to standard output use "-" (or the
   --  Standar_Output constant)
   --  The value returned by the script is ignored


   function Slurp (Filename : String) return Template_Type;
   -- Read a template from the specified file.  Useful in conjuction with
   -- Expand_Template above


private
   procedure Define (Interpreter : in out Interpreter_Type;
                     Name        : Id;
                     Value       : Symbols.Symbol_Value);

   type Compiled_Code is limited
      record
         Code : Protypo.Code_Trees.Parsed_Code;
      end record;


   type Interpreter_Type is tagged limited
      record
         Symbol_Table : Symbol_Tables.Symbol_Table_type;
      end record;

   function Slurp (Filename : String) return Template_Type
   is (Template_Type (Utilities.Slurp (Filename)));

   procedure Bye (X : in out Compiled_Code);


end Protypo.API.Interpreters;
