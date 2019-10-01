-- A string set is just a weaker form of regular expression.  A string
-- set is a finite set of strings where each character belongs to a set.
--
-- A string set can be represented by a regex of the form
--
--  [...][...][...]
--
-- that is, as a concatenation of character sets.
--
-- This is convenient choice when you do not need the full "weight" of
-- usual regexs.
with Ada.Strings.Maps;     use Ada.Strings.Maps;

package String_Sets is
   type Set_String (<>) is private;

   function To_Set_String (X : String) return Set_String;
   -- Create a string set that contains only X

   function "&" (X, Y : Set_String) return Set_String;

   function "&" (X : String;  Y : Character_Set) return Set_String;

   function Match (X : String; Pattern : Set_String) return Boolean;
private
   type Basic_Set_String is array (Positive range <>) of Character_Set;
   type Set_String is new Basic_Set_String;

   function "&" (X, Y : Set_String) return Set_String
   is (Set_String(Basic_Set_String (X) & Basic_Set_String (Y)));

   function "&" (X : String;  Y : Character_Set) return Set_String
   is (To_Set_String (X) & Set_String'(1 => Y));
end String_Sets;
