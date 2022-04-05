pragma Ada_2012;
with Ada.Numerics.Elementary_Functions;

package body Callbacks is
   use Ada.Numerics.Elementary_Functions;
   use Engine_Values;
   use Engine_Values.Engine_Value_Vectors;

   ---------
   -- Sin --
   ---------

   function Sin (X : Engine_Value_Vectors.Vector)
                 return Engine_Value_Vectors.Vector
   is
   begin
      return To_Vector (Create (Sin (Get_Float (X.First_Element))), 1);
   end Sin;

end Callbacks;
