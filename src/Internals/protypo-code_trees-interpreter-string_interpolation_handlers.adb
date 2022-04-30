pragma Ada_2012;
package body Protypo.Code_Trees.Interpreter.String_Interpolation_Handlers is

   ------------
   -- Create --
   ------------

   function Create (Interp : Interpreter_Access)
                    return Handlers.Function_Interface'Class
   is
   begin
      return String_Interpolator'(Status => Interp);
   end Create;

   -------------
   -- Process --
   -------------

   function Process
     (Fun       : String_Interpolator;
                     Parameter : Engine_Value_Array)
      return Engine_Value_Array
   is
      use type ada.Containers.Count_Type;
   begin
      if Parameter.Length /= 1 then
         -- This should never happen
         raise Program_Error;
      end if;

      if Parameter.First_Element.Class /= Text then
         raise Run_Time_Error with "String interpolation applied to non-string";
      end if;

      declare
         Input : constant String := Get_String (Parameter.First_Element);

         Interpolated : constant String := Do_Escape (Status => Fun.Status,
                                                      Input  => Input);

         Result : Engine_Value_Array;
      begin
         Result.Append (Create (Interpolated));

         return Result;
      end;
   end Process;

   ---------------
   -- Signature --
   ---------------

   function Signature
     (Fun : String_Interpolator) return Parameter_Lists.Parameter_Signature
   is (1 => Parameter_Lists.Mandatory);


end Protypo.Code_Trees.Interpreter.String_Interpolation_Handlers;
