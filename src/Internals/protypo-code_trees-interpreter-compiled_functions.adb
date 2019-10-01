pragma Ada_2012;
with Protypo.Code_Trees.Interpreter.Statements;
with Protypo.Code_Trees.Interpreter.Expressions;

package body Protypo.Code_Trees.Interpreter.Compiled_Functions is

   -------------
   -- Process --
   -------------

   overriding function Process (Fun       : Compiled_Function;
                                Parameter : Engine_Value_Array)
                                return Engine_Value_Array
   is
   begin
      if Parameter'Length /= Integer (Fun.Parameters.Names.Length) then
         raise Program_Error;
      end if;

      Fun.Status.Symbol_Table.Open_External_Block;

      declare
         Name_To_Param : constant Integer :=
                           Parameter'First - Fun.Parameters.Names.First_Index;
      begin
         for Name_Index in Fun.Parameters.Names.First_Index .. Fun.Parameters.Names.Last_Index loop
            Fun.Status.Symbol_Table.Create
              (Name          => Fun.Parameters.Names (Name_Index),
               Initial_Value => Parameter (Name_Index + Name_To_Param));
         end loop;
      end;


      Statements.Run (Fun.Status, Fun.Function_Body);

      case Fun.Status.Break.Breaking_Reason is
         when Exit_Statement =>
            raise Constraint_Error;

         when None =>
            return No_Value;

         when Return_Statement =>

            declare
               Result : constant Engine_Value_Array := Expressions.To_Array (Fun.Status.Break.Result);
            begin
               Fun.Status.Break := No_Break;
               return Result;
            end;
      end case;
   end Process;

   ------------------------
   -- Default_Parameters --
   ------------------------

   function Default_Parameters (Fun : Compiled_Function)
                                return Engine_Value_Array
   is
      Result : Engine_Value_Array (Fun.Parameters.Default.First_Index .. Fun.Parameters.Default.Last_Index);
   begin
      for K in Result'Range loop
         Result (K) := Expressions.Eval_scalar (Fun.Status, Fun.Parameters.Default (K));
      end loop;

      return Result;
   end Default_Parameters;


end Protypo.Code_Trees.Interpreter.Compiled_Functions;
