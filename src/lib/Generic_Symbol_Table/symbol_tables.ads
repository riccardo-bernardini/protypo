with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Containers;

with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Hash;

use Ada.Strings;

package Symbol_Tables is
   type ID_Type is new String
     with Dynamic_Predicate => Is_Valid_ID (String (ID_Type));

   function Is_Valid_ID (X : String) return Boolean
   is (X'Length > 0
       and then Is_Letter (X (X'First))
       and then (for all C of X => Is_Alphanumeric (C) or C = '_'));

   function Equivalent_Id (X, Y : ID_Type) return Boolean
   is (X = Y);

   function Equal_Case_Insensitive_Id (X, Y : ID_Type) return Boolean
   is (Equal_Case_Insensitive (String (X), String (Y)));


   function Hash_Id (X : ID_Type) return Ada.Containers.Hash_Type
   is (Hash (String (X)));

   function Hash_Case_Insensitive_ID (X : ID_Type) return Ada.Containers.Hash_Type
   is (Hash_Case_Insensitive (String (X)));
end Symbol_Tables;
