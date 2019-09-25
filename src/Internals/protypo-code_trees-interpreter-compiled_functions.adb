pragma Ada_2012;
package body Protypo.Code_Trees.Interpreter.Compiled_Functions is

   -------------
   -- Process --
   -------------

   overriding function Process (Fun       : Compiled_Function;
                                Parameter : Engine_Value_Array)
                                return Engine_Value_Array
   is
      D : constant Integer := Parameter'First - Fun.Parameters.Names.First_Index;
      E : constant Integer := Fun.Parameters.Default.First_Index - Fun.Parameters.Names.First_Index;
   begin
      if Parameter'Length > Integer (Fun.Parameters.Names.Length) then
         raise Constraint_Error;
      end if;

      Fun.Symbol_Table.Open_External_Block;

      for K in Fun.Parameters.Names.First_Index .. Fun.Parameters.Names.Last_Index loop
         if D + K <= Parameter'Last then
            Fun.Symbol_Table.Create (Name          => Fun.Parameters.Names (K),
                                     Initial_Value => Parameter (D + K));

         elsif E + K <= Fun.Parameters.Default.Last_Index then
            declare
               Default : constant Interpreter_Result :=
                           Run (Fun.Parameters.Default (E + K),
                                Fun.Symbol_Table);
            begin
               if Default.Reason /= Expression then
                  raise Program_Error;
               end if;


               Fun.Symbol_Table.Create (Name          => Fun.Parameters.Names (K),
                                        Initial_Value => Default.Value);
            end;

         else
            raise Constraint_Error;
         end if;
      end loop;

      declare
         Result : constant Interpreter_Result := Run (Program      => Fun.Function_Body,
                                                      Symbol_Table => Fun.Symbol_Table);
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
