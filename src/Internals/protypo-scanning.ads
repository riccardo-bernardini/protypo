with Readable_Sequences.Generic_Sequences;
with Protypo.Tokens;
with Protypo.Api.Interpreters;

private package Protypo.Scanning is
   type Token_Array is array (Positive range<>) of Tokens.Token;

   package Token_Sequences is
     new Readable_Sequences.Generic_Sequences (Element_Type  => Tokens.Token,
                                               Element_Array => Token_Array);

   subtype Token_List is Token_Sequences.Sequence;

   function Tokenize (Template : Protypo.Api.Interpreters.Template_Type;
                      Base_Dir : String) return Token_List;

   procedure Dump (Item : Token_List);
   -- Print to stdout the content of Item.  Useful for debugging

   Scanning_Error : exception;

   Consume_With_Escape_Procedure_Name : constant ID := "@@";
   Consume_Procedure_Name    : constant ID := "@";


end Protypo.Scanning;
