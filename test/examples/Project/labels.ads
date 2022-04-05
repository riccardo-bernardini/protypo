with Ada.Strings.Bounded;

package Labels is
   Max_Label_Length : constant := 128;

   package Bounded_Labels is
         new Ada.Strings.Bounded.Generic_Bounded_Length (Max_Label_Length);

   subtype Label_Type is Bounded_Labels.Bounded_String;

   function "+" (X : String) return Label_Type
   is (Bounded_Labels.To_Bounded_String (X));

   function "+" (X : Label_Type) return String
                 renames Bounded_Labels.To_String;
end Labels;
