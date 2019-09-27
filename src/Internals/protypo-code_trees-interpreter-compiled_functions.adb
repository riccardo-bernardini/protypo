pragma Ada_2012;
package body Protypo.Code_Trees.Interpreter.Compiled_Functions is

   -------------
   -- Process --
   -------------

   overriding function Process (Handler   : Compiled_Function;
                                Parameter : Engine_Value_Array)
                                return Engine_Value_Array
   is
   begin
      if Parameter'Length /= Integer (Fun.Parameters.Names.Length) then
         raise Program_Error;
      end if;

      Fun.Symbol_Table.Open_External_Block;

      declare
         Name_To_Param : constant Integer :=
                           Parameter'First - Fun.Parameters.Names.First_Index;
      begin
         for Name_Index in Handler.Parameters.Names.First_Index .. Handler.Parameters.Names.Last_Index loop
            Handler.Symbol_Table.Create
                  (Name          => Handler.Parameters.Names (Name_Index),
                   Initial_Value => Parameter (Name_Index + Name_To_Param));
         end loop;
      end;


      declare
         Result : constant Interpreter_Result :=
                    Run (Program      => Handler.Function_Body,
                         Symbol_Table => Handler.Symbol_Table);
      begin
         Fun.Symbol_Table.Close_Block;

         case Result.Reason is
            when End_Of_Code =>
               return No_Value;

            when Return_Statement =>
               return To_Array (Result.Result);

            when Exit_Statement | Expression =>
               raise Constraint_Error;
         end case;
      end;
   end Process;

end Protypo.Code_Trees.Interpreter.Compiled_Functions;
