with Protypo.Scanning;
with Protypo.Code_Trees;

package Protypo.Parsing is
   function Parse_Statement_Sequence (Input : Scanning.Token_List)
                                      return Code_Trees.Parsed_Code;
end Protypo.Parsing;
