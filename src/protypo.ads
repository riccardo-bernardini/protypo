with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
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

   subtype ID is String
     with Dynamic_Predicate => Is_Valid_Id (ID);

   subtype Unbounded_ID is Strings.Unbounded.Unbounded_String
     with Dynamic_Predicate => Is_Valid_Id (Strings.Unbounded.To_String (Unbounded_ID));

end Protypo;
