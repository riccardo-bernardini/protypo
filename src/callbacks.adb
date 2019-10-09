pragma Ada_2012;
with Ada.Numerics.Elementary_Functions;

package body Callbacks is
   use Ada.Numerics;
   ---------
   -- Sin --
   ---------

   function Sin (X : Engine_Values.Engine_Value_Array)
                 return Engine_Values.Engine_Value_Array
   is
   begin
      return (1 => Engine_Values.Create
              (Elementary_Functions.Sin (Engine_Values.Get_Float (X (X'First)))));
   end Sin;

end Callbacks;
