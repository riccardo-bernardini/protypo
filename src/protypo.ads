with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;

with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;

package Protypo is
   use Ada;

   use type Ada.Strings.Maps.Character_Set;

   Run_Time_Error : exception;
   Parsing_Error  : exception;

   Begin_Id_Set   : constant Strings.Maps.Character_Set :=
                      Strings.Maps.Constants.Letter_Set or Strings.Maps.To_Set ("@");

   Id_Charset     : constant Strings.Maps.Character_Set :=
                      Begin_Id_Set
                          or Strings.Maps.Constants.Decimal_Digit_Set
                              or Strings.Maps.To_Set ("_");


   function Is_Valid_Id (ID : String) return Boolean
   is (ID /= "" and then
         (Strings.Maps.Is_In (ID (ID'First), Begin_Id_Set)) and then
         (for all C of ID => Strings.Maps.Is_In (C, Id_Charset)));

   type ID is new String
     with Dynamic_Predicate => Is_Valid_Id (String(ID));

   function "<" (X, Y : Id) return Boolean
   is (Strings.Less_Case_Insensitive (String (X), String (Y)));

   function "=" (X, Y : Id) return Boolean
   is (Strings.Equal_Case_Insensitive (String (X), String (Y)));

   type Unbounded_ID is new Strings.Unbounded.Unbounded_String
     with Dynamic_Predicate =>
       Is_Valid_Id
         (Strings.Unbounded.To_String
            (Strings.Unbounded.Unbounded_String (Unbounded_ID)));

   function To_String (X : Unbounded_ID) return string
   is (Strings.Unbounded.To_String(Strings.Unbounded.Unbounded_String (x)));

   function To_Id (X : Unbounded_ID) return Id
   is (ID (To_String (x)));

   function "<" (X, Y : Unbounded_ID) return Boolean
   is (To_Id (X) < To_Id (Y));

   function "=" (X, Y : Unbounded_ID) return Boolean
   is (To_Id (X) = To_Id (Y));

end Protypo;
