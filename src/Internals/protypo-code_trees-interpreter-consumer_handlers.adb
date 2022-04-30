pragma Ada_2012;
--  with Readable_Sequences.String_Sequences;
--  use Readable_Sequences.String_Sequences;

--  with Protypo.Scanning;
--  with Protypo.Parsing;
--  with Protypo.Code_Trees.Interpreter.Expressions;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_Io; use Ada.Text_Io;
--  with Protypo.Api.Interpreters;

package body Protypo.Code_Trees.Interpreter.Consumer_Handlers is

   function To_String (X : Engine_Value) return String
   is (case X.Class is
          when Int    => Get_Integer (X)'Image,
          when Real   => Get_Float (X)'Image,
          when Text   => Get_String (X),
          when others => raise Constraint_Error);

   overriding procedure Process (Fun       : Consumer_Callback;
                                 Parameter : Engine_Value_Array)
   is

   begin
      for P of Parameter loop
         declare
            To_Be_Consumed : constant String :=
                               (if Fun.With_Escape
                                then
                                   Do_Escape (Fun.Status, To_String (P))
                                else
                                   To_String (P));
         begin
            Fun.Consumer.Process (To_Be_Consumed & To_String (Fun.End_Of_Line));
         end;
      end loop;
   end Process;

   function Signature (Fun : Consumer_Callback)
                       return Api.Engine_Values.Parameter_Lists.Parameter_Signature
   is (1 => Parameter_Lists.Mandatory);


end Protypo.Code_Trees.Interpreter.Consumer_Handlers;
