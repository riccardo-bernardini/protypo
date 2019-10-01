pragma Ada_2012;
with Protypo.Api.Engine_Values;  use Protypo.Api.Engine_Values;
with Protypo.Code_Trees.Interpreter.Consumer_Handlers;
with Protypo.Code_Trees.Interpreter.Statements;
with Ada.Text_IO; use Ada.Text_IO;

package body Protypo.Code_Trees.Interpreter is
   ---------
   -- Run --
   ---------

   procedure Run
     (Program      : Parsed_Code;
      Symbol_Table : Api.Symbols.Table;
      Consumer     : Api.Consumers.Consumer_Access)
   is
      use Api.Symbols;

      procedure Add_Builtin_Values (Table    : in out Api.Symbols.Table)
      is
      begin
         Table.Create (Name          => "consume",
                       Initial_Value => Create (Consumer_Handlers.Create (Consumer)));
      end Add_Builtin_Values;

      Interpreter : constant Interpreter_Access :=
                      new Interpreter_Type'(Break        => No_Break,
                                            Symbol_Table => Copy_Globals (Symbol_Table));
   begin
      Add_Builtin_Values (Interpreter.Symbol_Table);

      PUt_Line ("xxx");
      Statements.Run (Interpreter, Program.Pt);
      PUt_Line ("BBB");

      if Interpreter.Break /= No_Break  then
         raise Program_Error;
      end if;
      PUt_Line ("CCC");
   end Run;

end Protypo.Code_Trees.Interpreter;


 --     function "+" (X : Engine_Value_Vectors.Vector)
   --                   return Engine_Value
   --           with
   --                 Pre => X.Length = 1;

   --     function "+" (X : Engine_Value_Vectors.Vector)  return Engine_Value
   --     is (X.First_Element);

   --     function "+" (X : Engine_Value)  return Engine_Value_Vectors.Vector
   --     is
   --        Result : Engine_Value_Vectors.Vector;
   --     begin
   --        Result.Append (X);
   --        return Result;
   --     end "+";

   --     function "+" (X : Engine_Value)  return Engine_Value_Array
   --     is (To_Array (+X));
   --
   --     function "+" (X : Engine_Value_Array)  return Engine_Value
   --     is (if X'Length = 1 then
   --            X (X'First)
   --         else
   --            raise Constraint_Error);

