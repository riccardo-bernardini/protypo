with Protypo.Api.Interpreter;
with Protypo.Api.Symbols;
with Protypo.Api.Consumers.File_Writer;
--  with Protypo.Api.Engine_Values.List_Wrappers;
with Protypo.Api.Engine_Values.Array_Wrappers;

with Utilities;
with User_Records;

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Protypo.Api.Engine_Values;

procedure Prova_Interpreter is
   use Ada.Command_Line;

   use Protypo.Api.Interpreter;
   use Protypo.Api.Engine_Values;
   use Protypo.Api.Consumers;
   use Protypo.Api;

   use User_Records;
   use User_Records.User_Record_Package;

   type Multi_Aggregate is array (Positive range <>) of Aggregate_Type;

   function Source_File return String
   is (if Argument_Count = 0 then
          "test-data/interprete.txt"
       else
          Argument (1));

   --------------
   -- To_Array --
   --------------

   function To_Array (Db : Multi_Aggregate) return Engine_Value_Array
   is
      Result : Engine_Value_Array (Db'Range);
   begin
      for K in Db'Range loop
         Result (K) := Create (Record_Interface_Access (Make_Record (Db (K))));
      end loop;

      return Result;
   end To_Array;

   Program  : constant String := Utilities.Slurp (Source_File);
   Code     : Compiled_Code;
   Table    : Symbols.Table;
   Consumer : constant Consumer_Access :=
                File_Writer.Open (File_Writer.Standard_Error);


   Db : constant Multi_Aggregate :=
          (1 => (First_Name => Create ("Pippo"),
                 Last_Name  => Create ("Recupero"),
                 Telephone  => Create ("3204365972")),
           2 => (First_Name => Create ("Diego"),
                 Last_Name  => Create ("della Vega"),
                 Telephone  => Create ("zzzzzz")),
           3 => (First_Name => Create ("Topolino"),
                 Last_Name  => Create ("de Topis"),
                 Telephone  => Create ("12345")),
           4 => (First_Name => Create ("Paolino"),
                 Last_Name  => Create ("Paperino"),
                 Telephone  => Create ("1313")));

   User_Dir : constant Array_Wrappers.Array_Wrapper_Access :=
                Array_Wrappers.Make_Wrapper (To_Array (Db));

begin


   Table.Create (Name          => "users",
                 Initial_Value => Create (Ambivalent_Interface_Access (User_Dir)));

   Compile (Target   => Code,
            Program  => Program);

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
