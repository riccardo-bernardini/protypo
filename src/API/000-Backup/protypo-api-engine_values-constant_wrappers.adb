pragma Ada_2012;
--  with Ada.Text_Io; use Ada.Text_Io;

package body Protypo.Api.Engine_Values.Constant_Wrappers is

   ----------------
   -- To_Handler --
   ----------------

   function To_Handler_Value (Value : Engine_Value) return Handler_Value is
   begin
      if Value in Handler_Value then
         return value;
      else
         declare
            Result : constant Engine_Value :=
                       Handlers.Create (Handlers.Constant_Interface_Access (Make_Wrapper (Value)));
         begin
            --  Put_Line (Result.Class'Image);
            --  Put_Line (Boolean'image(Result.Class in Handler_Classes));
            --  Put_Line (Boolean'Image (Result in Handler_Value));
            return Result;
         end;
      end if;
   end To_Handler_Value;

end Protypo.Api.Engine_Values.Constant_Wrappers;
