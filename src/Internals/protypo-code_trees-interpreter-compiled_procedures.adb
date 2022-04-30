pragma Ada_2012;
with Protypo.Code_Trees.Interpreter.Statements;
with Protypo.Code_Trees.Interpreter.Expressions;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_IO; use Ada.Text_IO;

package body Protypo.Code_Trees.Interpreter.Compiled_procedures is

   -------------
   -- Process --
   -------------

   overriding procedure Process (Fun       : Compiled_procedure;
                                 Parameter : Engine_Value_Array)
   is
      use type ada.Containers.Count_Type;
   begin
      if Parameter.Length /= Fun.Parameters.Names.Length then
         raise Program_Error;
      end if;

      Fun.Status.Symbol_Table.Open_External_Namespace;

      declare
         Name_To_Param : constant Integer :=
                           Parameter.First_Index - Fun.Parameters.Names.First_Index;
      begin
         for Name_Index in Fun.Parameters.Names.First_Index .. Fun.Parameters.Names.Last_Index loop
            Fun.Status.Symbol_Table.Define_Variable
              (Name  => Fun.Parameters.Names (Name_Index),
               Value => Parameter (Name_Index + Name_To_Param));
         end loop;
      end;


      Statements.Run (Fun.Status, Fun.procedure_Body);

      Fun.Status.Symbol_Table.Close_Namespace;

      case Fun.Status.Break.Breaking_Reason is
         when Exit_Statement =>
            raise Constraint_Error;

         when None =>
            null;

         when Return_Statement =>

            declare
               Result : constant Engine_Value_Array := Fun.Status.Break.Result;
            begin
               if not Result.Is_Empty then
                  raise Program_Error
                    with "A procedure returned a value";
               end if;

               Fun.Status.Break := No_Break;
            end;
      end case;
   end Process;

   function Signature (Fun : Compiled_procedure)
                       return Api.Engine_Values.Parameter_Lists.Parameter_Signature
   is

      Result : Parameter_Lists.Parameter_Signature (Fun.Parameters.Default.First_Index .. Fun.Parameters.Default.Last_Index);
   begin
      for K in Result'Range loop
         if Fun.Parameters.Default (K) /= null then
            Result (K) := Parameter_Lists.Optional (Expressions.Eval_Scalar (Fun.Status, Fun.Parameters.Default (K)));
         else
            Result (K) := Parameter_Lists.Mandatory;
         end if;
      end loop;

      return Result;
   end Signature;


end Protypo.Code_Trees.Interpreter.Compiled_procedures;
