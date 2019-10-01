with Protypo.Api.Interpreter;
with Protypo.Api.Symbols;
with Protypo.Api.Consumers.File_Writer;

--  with Utilities;


procedure Prova_Interpreter is
   use Protypo.Api.Interpreter;
   use Protypo.Api.Consumers;
   use Protypo.Api;

   S    : constant String := "pippo pluto e paperino #{ x := 2+3*4; }# #x";
   Code : constant Compiled_Code := Compile (S);
   Table : Symbols.Table;
   Consumer : constant Consumer_Access := File_Writer.Open (File_Writer.Standard_Error);
begin
   Run (Program      => Code,
        Symbol_Table => Table,
        Consumer     => Consumer);
end Prova_Interpreter;
