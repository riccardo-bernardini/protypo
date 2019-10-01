pragma Ada_2012;
package body Protypo.Code_Trees.Interpreter.Consumer_Handlers is



   overriding function Process (Fun       : Consumer_Callback;
                                Parameter : Engine_Value_Array)
                                return Engine_Value_Array
   is
      function To_String (X : Engine_Value) return String
      is (case X.Class is
             when Int    => Get_Integer (X)'Image,
             when Real   => Get_Float (X)'Image,
             when Text   => Get_String (X),
             when others => raise Constraint_Error);

   begin
      for P of Parameter loop
         Fun.Consumer.Process (To_String (P));
      end loop;

      return No_Value;
   end Process;

   function Default_Parameters (Fun : Consumer_Callback)
                                return Engine_Value_Array
   is (1 => Void_Value);


end Protypo.Code_Trees.Interpreter.Consumer_Handlers;
