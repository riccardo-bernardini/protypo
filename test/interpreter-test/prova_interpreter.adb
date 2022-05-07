with Protypo.Api.Interpreters;
with Protypo.Api.Consumers.File_Writer;
with Protypo.Api.Engine_Values.Engine_Vector_Handlers;
with Protypo.Api.Engine_Values.Engine_Value_Holders;

with User_Records;
with Integer_Arrays;

with Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Exceptions;
with Protypo.Api.Engine_Values;
with Protypo.Api.Engine_Values.Handlers;
use Protypo.Api.Engine_Values.Handlers;

procedure Prova_Interpreter is
   use Protypo.Api.Engine_Values;


   use Ada.Command_Line;

   use Protypo.Api.Interpreters;
   use Protypo.Api.Engine_Values;
   use Protypo.Api.Consumers;
   use Protypo.Api;

   use User_Records;
   use User_Records.User_Record_Package;


   function Source_File return String
   is (if Argument_Count = 0 then
          "test-data/interprete.txt"
       else
          Argument (1));

   function Create_Holder (X : String) return Engine_Value_Holders.Holder
   is (Engine_Value_Holders.To_Holder (Create (X)));


   Program  : constant Template_Type := Slurp (Source_File);
   Code     : Compiled_Code;
   Consumer : constant Consumer_Access :=
                File_Writer.Open (File_Writer.Standard_Error_Special_Name);


   Db : constant Multi_Aggregate :=
          (1 => (First_Name => Create_Holder ("Pippo"),
                 Last_Name  => Create_Holder ("Recupero"),
                 Telephone  => Create_Holder ("3204365972")),
           2 => (First_Name => Create_Holder ("Diego"),
                 Last_Name  => Create_Holder ("della Vega"),
                 Telephone  => Create_Holder ("zzzzzz")),
           3 => (First_Name => Create_Holder ("Topolino"),
                 Last_Name  => Create_Holder ("de Topis"),
                 Telephone  => Create_Holder ("12345")),
           4 => (First_Name => Create_Holder ("Paolino"),
                 Last_Name  => Create_Holder ("Paperino"),
                 Telephone  => Create_Holder ("1313")));

   Scores : constant Engine_Value :=
              Integer_Arrays.Wrappers.Create (Value => (1 => 18,
                                                        2 => 33,
                                                        3 => 42,
                                                        4 => -5));

   User_Dir : constant Ambivalent_Interface_Access :=
                Ambivalent_Interface_Access (Engine_Vector_Handlers.To_Vector_Handler (To_Array (Db)));

   Engine   : Interpreter_Type;

begin

   Engine.Define (Name  => "users",
                  Value => Create (User_Dir));

   Engine.Define (Name => "scores",
                  Value => Scores);

   Engine.Define_Function (Name  => "splitbit",
                  Funct => Handlers.Create (Val            => User_Records.Split_Bit'Access,
                                            Min_Parameters => 1,
                                            Max_Parameters => 2));

   Compile (Target   => Code,
            Program  => Program);

   Engine.Run (Program      => Code,
               Consumer     => Consumer);

   Set_Exit_Status (Success);
exception
   when E : Protypo.Run_Time_Error =>
      Put_Line (Standard_Error, "Run time error: " & Ada.Exceptions.Exception_Message (E));

      Set_Exit_Status (Failure);

   when E : Protypo.Parsing_Error =>
      Put_Line (Standard_Error, "parsing error: " & Ada.Exceptions.Exception_Message (E));

      Set_Exit_Status (Failure);

   when E : others =>
      Put_Line (Standard_Error, "other: " & Ada.Exceptions.Exception_Message (E)
                & Ada.Exceptions.Exception_Information (E));
end Prova_Interpreter;
