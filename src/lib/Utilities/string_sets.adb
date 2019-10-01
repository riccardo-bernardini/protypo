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


   -----------
   -- Match --
   -----------

   function Match (X : String; Pattern : Set_String) return Boolean
   is
   begin
      if X'Length < Pattern'Length then
         return False;
      end if;

      for K in Pattern'Range loop
         if not Is_In (X (K - Pattern'First + X'First), Pattern (K)) then
            return False;
         end if;
      end loop;

      return True;
   end Match;

end String_Sets;
