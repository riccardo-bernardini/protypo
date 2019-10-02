with Protypo.Api.Interpreter;
with Protypo.Api.Symbols;
with Protypo.Api.Consumers.File_Writer;

with Utilities;

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

procedure Prova_Interpreter is
   use Ada.Command_Line;

   use Protypo.Api.Interpreter;
   use Protypo.Api.Consumers;
   use Protypo.Api;

   function Source_File return String
   is (if Argument_Count = 0 then
          "test-data/interprete.txt"
       else
          Argument (1));

   Program  : constant String := Utilities.Slurp (Source_File);
   --"pippo pluto e paperino #{ c:=9; x := 2+3*c; }# #x#c";
   Code     : Compiled_Code;
   Table    : Symbols.Table;
   Consumer : constant Consumer_Access :=
                File_Writer.Open (File_Writer.Standard_Error);

begin
   Compile (Target   => Code,
            Program  => Program);
   Dump (Code);

   Run (Program      => Code,
        Symbol_Table => Table,
        Consumer     => Consumer);

   Set_Exit_Status (Success);
exception
   when E : Protypo.Run_Time_Error =>
      Put_Line (Standard_Error, "Run time error: " & Ada.Exceptions.Exception_Message (E));

      Set_Exit_Status (Failure);

   when E : Protypo.Parsing_Error =>
      Put_Line (Standard_Error, "parsing error: " & Ada.Exceptions.Exception_Message (E));

      Set_Exit_Status (Failure);
end Prova_Interpreter;
