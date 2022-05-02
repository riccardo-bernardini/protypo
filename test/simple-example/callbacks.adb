pragma Ada_2012;
with Ada.Numerics.Elementary_Functions;

package body Callbacks is
   use Ada.Numerics.Elementary_Functions;
   use Engine_Values;

   ---------
   -- Sin --
   ---------

   function Sin (X : Engine_Value_Array)
                 return Engine_Value_Array
   is
   begin
      return Singleton (Create (Sin (Get_Float (X.First_Element))));
   end Sin;

end Callbacks;
