pragma Ada_2012;
package body String_Sets is

   -------------------
   -- To_Set_String --
   -------------------

   function To_Set_String (X : String) return Set_String
   is
      Result : Set_String (X'Range);
   begin
      for K in X'Range loop
         Result (K) := To_Set (X (K));
      end loop;

      return Result;
   end To_Set_String;

end String_Sets;
