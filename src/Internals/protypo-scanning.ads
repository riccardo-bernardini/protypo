with Ada.Strings.Maps.Constants;     use Ada.Strings.Maps;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;

with Readable_Sequences.Generic_Sequences;
with Protypo.Tokens;

private package Protypo.Scanning is
   Id_Charset   : constant Character_Set := Constants.Alphanumeric_Set or To_Set ("_@");
   Begin_Id_Set : constant Character_Set := Constants.Letter_Set or To_Set ("@");

   function Is_Valid_Id (ID : String) return Boolean
   is (ID /= "" and then
         (Is_In (ID (ID'First), Begin_Id_Set)) and then
         (for all C of ID => Is_In (C, Id_Charset)));

   subtype ID is String
     with Dynamic_Predicate => Is_Valid_Id (ID);

   subtype Unbounded_ID is Unbounded_String
     with Dynamic_Predicate => Is_Valid_Id (To_String (Unbounded_ID));

   type Token_Array is array (Positive range<>) of Tokens.Token;

   package Token_Sequences is
     new Readable_Sequences.Generic_Sequences (Element_Type  => Tokens.Token,
                                               Element_Array => Token_Array);

   subtype Token_List is Token_Sequences.Sequence;

   function Tokenize (Template : String;
                      Base_Dir : String) return Token_List;

   procedure Dump (Item : Token_List);
   -- Print to stdout the content of Item.  Useful for debugging

   Scanning_Error : exception;

   Consume_With_Escape_Procedure_Name : constant ID := "@@";
   Consume_Procedure_Name    : constant ID := "@";


end Protypo.Scanning;
