with Protypo.Api.Interpreters;
with Protypo.Api.Consumers.File_Writer;
with Protypo.Api.Engine_Values.Basic_Array_Wrappers;

-- with User_Records;
with Integer_Arrays;

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Protypo.Api.Engine_Values.Table_Wrappers;

procedure Test_Tabelle is
   use Ada.Command_Line;

   use Protypo.Api.Interpreters;
   use Protypo.Api.Engine_Values;
   use Protypo.Api.Consumers;
   use Protypo.Api;

   Default_Source_File : constant String := "test-data/prova_tabelle.txt";

   function Source_File return String
   is (if Argument_Count = 0 then Default_Source_File else Argument (1));

   use Protypo.Api.Engine_Values;

   type User_Record is
      record
         Age     : Integer;
         Serial  : Integer;
         Address : Integer;
      end record;

   type User_Field is (Age, Serial, Address);

   package User_Package is
         new Table_Wrappers.Enumerated_Rows (User_Field);

   type Aggregate_Array is array (Positive range <>) of User_Record;

   function Convert (X : User_Record) return User_Package.Aggregate_Type
   is

   begin
      return User_Package.Aggregate_Type'(Age     => Create (X.Age),
                                          Serial  => Create (X.Serial),
                                          Address => Create (X.Address));
   end Convert;

   procedure Fill is
         new User_Package.Append_Array (Ada_Aggregate           => User_Record,
                                        Aggregate_Index         => Positive,
                                        Generic_Aggregate_Array => Aggregate_Array,
                                        Convert                 => Convert);



   DB : constant Aggregate_Array := (1 => (Age     => 42,
                                           Serial  => 12345,
                                           Address => 99),

                                     2 => (Age     => 53,
                                           Serial  => 23451,
                                           Address => 0),

                                     3 => (Age     => 66,
                                           Serial  => 34512,
                                           Address => 44),

                                     4 => (Age     => 88,
                                           Serial  => 54321,
                                           Address => 112));

--     Program  : constant Template_Type := Slurp (Source_File);
   Code     : Compiled_Code;
   Consumer : constant Consumer_Access :=
                File_Writer.Open (File_Writer.Standard_Error);


   Scores : constant Engine_Value :=
              Integer_Arrays.Wrappers.Create (Value => (1 => 18,
                                                        2 => 33,
                                                        3 => 42,
                                                        4 => -5));

   User_Dir : constant Table_Wrappers.Table_Wrapper_Access :=
                User_Package.Make_Table;

   Engine   : Interpreter_Type;

begin
   Fill (User_Dir.all, Db);

   Engine.Define (Name  => "users",
                  Value => Create (Ambivalent_Interface_Access (User_Dir)));

   Engine.Define (Name => "scores",
                  Value => Scores);

   --     Engine.Define (Name  => "splitbit",
   --                    Value => Create (Val            => User_Records.Split_Bit'Access,
   --                                     Min_Parameters => 1,
   --                                     Max_Parameters => 2));

   Compile (Target   => Code,
            Program  => Slurp(Source_File));

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
      Put_Line (Standard_Error, "other: " & Ada.Exceptions.Exception_Message (E));
      Put_Line (Ada.Exceptions.Exception_Information (E));
end Test_Tabelle;
