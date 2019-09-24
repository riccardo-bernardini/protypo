with Protypo.Scanning;
with Protypo.Code_Trees;

procedure Protypo.Parsing.Test is
   S    : constant String := "#{ a := b + 3; }#";
   Tk   : Scanning.Token_List := Scanning.Tokenize (S, "");
   Code : constant Code_Trees.Parsed_Code := Parse_Statement_Sequence (Tk);
begin
   Scanning.Dump (Tk);

   Code_Trees.Dump (Code);
end Protypo.Parsing.Test;
