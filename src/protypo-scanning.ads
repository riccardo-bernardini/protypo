with Readable_Sequences.Generic_Sequences;
with Protypo.Tokens;

package Protypo.Scanning is
   type Token_Array is array (Positive range<>) of Tokens.Token;

   package Token_Sequences is
     new Readable_Sequences.Generic_Sequences (Element_Type  => Tokens.Token,
                                               Element_Array => Token_Array);

   subtype Token_List is Token_Sequences.Sequence;

   function Tokenize (Input : String) return Token_List;

end Protypo.Scanning;
