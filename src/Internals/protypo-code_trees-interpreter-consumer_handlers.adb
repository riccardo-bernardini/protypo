pragma Ada_2012;
with Readable_Sequences.String_Sequences;
use Readable_Sequences.String_Sequences;

with Protypo.Scanning;
with Protypo.Parsing;
with Protypo.Code_Trees.Interpreter.Expressions;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_IO; use Ada.Text_IO;
with Protypo.Api.Interpreters;

package body Protypo.Code_Trees.Interpreter.Consumer_Handlers is

   function To_String (X : Engine_Value) return String
      is (case X.Class is
             when Int    => Get_Integer (X)'Image,
             when Real   => Get_Float (X)'Image,
             when Text   => Get_String (X),
             when others => raise Constraint_Error);

   --------------------
   -- Parse_And_Eval --
   --------------------

   function Parse_And_Eval (Status : Interpreter_Access;
                            Input  : String)
                            return Engine_Value
   is
      Tk : Scanning.Token_List := Scanning.Tokenize (Api.Interpreters.Template_Type ("#{" & Input & "}#"), "");
      Code : constant Code_Trees.Parsed_Code := Parsing.Parse_Expression (Tk);
   begin
--        Scanning.Dump (Tk);
--        Code_Trees.Dump (Code);

      return Expressions.Eval_Scalar (Status, Code.pt);
   end Parse_And_Eval;

   ---------------
   -- Do_Escape --
   ---------------

   function Do_Escape (Status : Interpreter_Access;
                       Input  : String)
                       return String
   is
      type Automata_State is (Reading_Text, Reading_Expression);

      EOF : constant Character := Character'Val (0);
      Seq : Sequence := Create (Input, EOF);
      Current_State : Automata_State := Reading_Text;

      Result : Sequence;
      Expr : Sequence;
   begin
      while not Seq.End_Of_Sequence loop
         case Current_State is
            when Reading_Text =>

               if Seq.Read = '#' and Seq.Read (1) /= '#' then
                  Seq.Next;
                  Expr.Clear;

                  Current_State := Reading_Expression;
               elsif Seq.Read = '#' and Seq.Read (1) = '#' then
                  Seq.Next (2);

                  Result.Append ('#');
               else
                  Result.Append (seq.Next);
               end if;

            when Reading_Expression =>

               if Seq.Read = '#' then
                  Seq.Next;

                  Current_State := Reading_Text;

--                    Put_Line ("<" & Expr.Dump & ">");
                  Result.Append (To_String (Parse_And_Eval (Status, Expr.Dump)));
               else
                  Expr.Append (Seq.Next);
               end if;
         end case;
      end loop;

      return Result.Dump;
   end Do_Escape;

   overriding function Process (Fun       : Consumer_Callback;
                                Parameter : Engine_Value_Array)
                                return Engine_Value_Array
   is

   begin
      for P of Parameter loop
         declare
            To_Be_Consumed : constant String := (if Fun.With_Escape then
                                                    Do_Escape (Fun.Status, To_String (P))
                                                 else
                                                    To_String (P));
         begin
            Fun.Consumer.Process (To_Be_Consumed);
         end;
      end loop;

      return No_Value;
   end Process;

   function Signature (Fun : Consumer_Callback)
                                return Api.Engine_Values.Parameter_Signature
   is (1 => Parameter_Spec'(Class   => Mandatory));


end Protypo.Code_Trees.Interpreter.Consumer_Handlers;
