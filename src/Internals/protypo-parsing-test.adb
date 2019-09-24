with Protypo.Scanning;
with Protypo.Code_Trees;

with Utilities;

procedure Protypo.Parsing.Test is
   S    : constant String := Utilities.Slurp ("test-data/parsing.txt");
   Tk   : Scanning.Token_List := Scanning.Tokenize (S, "");
   Code : Code_Trees.Parsed_Code;
--     pragma Unreferenced (Code);
begin
   Scanning.Dump (Tk);

   Code   := Parse_Statement_Sequence (Tk);
   Code_Trees.Dump (Code);
end Protypo.Parsing.Test;
