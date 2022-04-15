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

   End_Id_Set     : constant Strings.Maps.Character_Set :=
                      Id_Charset or Strings.Maps.To_Set ("?");

   function Is_Valid_Id (Id : String) return Boolean;


   type Id is new String
     with Dynamic_Predicate => Is_Valid_Id (String (Id));

   function "<" (X, Y : Id) return Boolean
   is (Strings.Less_Case_Insensitive (String (X), String (Y)));

   function "=" (X, Y : Id) return Boolean
   is (Strings.Equal_Case_Insensitive (String (X), String (Y)));

   type Unbounded_Id is new Strings.Unbounded.Unbounded_String
     with Dynamic_Predicate =>
       Is_Valid_Id
         (Strings.Unbounded.To_String
            (Strings.Unbounded.Unbounded_String (Unbounded_Id)));

   function To_String (X : Unbounded_Id) return String
   is (Strings.Unbounded.To_String (Strings.Unbounded.Unbounded_String (X)));

   function To_Id (X : Unbounded_Id) return Id
   is (Id (To_String (X)));

   function "<" (X, Y : Unbounded_Id) return Boolean
   is (To_Id (X) < To_Id (Y));

   function "=" (X, Y : Unbounded_Id) return Boolean
   is (To_Id (X) = To_Id (Y));

end Protypo;
