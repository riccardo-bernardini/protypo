with Protypo.Scanning;
with Protypo.Code_Trees;

private
package Protypo.Parsing is
   function Parse_Statement_Sequence (Input : in out Scanning.Token_List)
                                      return Code_Trees.Parsed_Code;
end Protypo.Parsing;
