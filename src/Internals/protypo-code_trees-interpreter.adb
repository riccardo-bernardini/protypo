pragma Ada_2012;
with Protypo.Api.Engine_Values;  use Protypo.Api.Engine_Values;
with Protypo.Code_Trees.Interpreter.Consumer_Handlers;
with Protypo.Code_Trees.Interpreter.Statements;
with Protypo.Code_Trees.Interpreter.Range_Iterators;
with Protypo.Scanning;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_IO; use Ada.Text_IO;

package body Protypo.Code_Trees.Interpreter is
   function To_String (X : Engine_Value) return String
      is (case X.Class is
             when Int    => Get_Integer (X)'Image,
             when Real   => Get_Float (X)'Image,
             when Text   => Get_String (X),
             when others => X.Class'Image);

   --------------------
   -- Range_Callback --
   --------------------

   function Range_Callback (Parameters : Engine_Value_Array)
                            return Engine_Value_Array
   is
   begin
      if Parameters'Length /= 2 then
         raise Run_Time_Error with "range needs 2 parameters";
      end if;

      return (1 => Create (Range_Iterators.Create
              (Start => Get_Integer (Parameters (Parameters'First)),
               Stop  => Get_Integer (Parameters (Parameters'First + 1)))));
   end Range_Callback;

   ---------
   -- Run --
   ---------

   procedure Run
     (Program      : Parsed_Code;
      Symbol_Table : Api.Symbols.Table;
      Consumer     : Api.Consumers.Consumer_Access)
   is
      use Api.Symbols;

      procedure Add_Builtin_Values (Table    : in out Api.Symbols.Table;
                                    Inter    : Interpreter_Access)
      is
      begin
         Table.Create
           (Name          => Scanning.Consume_Procedure_Name,
            Initial_Value => Create (Consumer_Handlers.Create (Consumer    => Consumer,
                                                               With_Escape => False,
                                                               Status      => Inter)));

         Table.Create
           (Name          => Scanning.Consume_With_Escape_Procedure_Name,
            Initial_Value => Create (Consumer_Handlers.Create (Consumer    => Consumer,
                                                               With_Escape => True,
                                                               Status      => Inter)));
         Table.Create
           (Name          => "range",
            Initial_Value => Create (Range_Callback'Access, 2));
      end Add_Builtin_Values;

      Interpreter : constant Interpreter_Access :=
                      new Interpreter_Type'(Break        => No_Break,
                                            Symbol_Table => Copy_Globals (Symbol_Table));
   begin
      --        Code_Trees.Dump (Program);
      Api.Symbols.Protypo_Tables.Set_Printer (To_String'Access);

      Add_Builtin_Values (Interpreter.Symbol_Table, Interpreter);

      Statements.Run (Interpreter, Program.Pt);

      if Interpreter.Break /= No_Break  then
         raise Program_Error;
      end if;
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

