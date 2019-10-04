pragma Ada_2012;
package body Protypo.Api.Engine_Values.Constant_Wrappers is

   ----------------
   -- To_Handler --
   ----------------

   function To_Handler_Value (Value : Engine_Value) return Handler_Value is
   begin
      if Value in Handler_Value then
         return value;
      else
         return Create (Constant_Interface_Access (Make_Wrapper (Value)));
      end if;
   end To_Handler_Value;

end Protypo.Api.Engine_Values.Constant_Wrappers;
