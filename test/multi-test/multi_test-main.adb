with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Directories;

with Protypo.Api.Interpreters;
with Protypo.Api.Engine_Values.Handlers;
with Protypo.Api.Consumers.File_Writer;

with Multi_Test.Test_Results;

use Ada;
use Protypo.Api;
with Ada.Text_Io; use Ada.Text_Io;

with Protypo.Api.Engine_Values.Array_Wrappers;

procedure Multi_Test.Main is
   type Integer_Array is array (Positive range <>) of Integer;

   package Integer_Array_Handlers is
     new Protypo.Api.Engine_Values.Array_Wrappers
       (Element_Type => Integer,
        Index_Type   => Positive,
        Array_Type   => Integer_Array,
        Create       => Engine_Values.Create,
        Get_Value    => Engine_Values.Get_Integer,
        Image        => Engine_Values.Image,
        Name         => "integer array");

   Bad_Command_Line : exception;

   function Data_Directory return String
   is
   begin
      case Argument_Count is
         when 0 =>
            return "multi_test_data";

         when 1 =>
            return Argument (1);

         when others =>
            raise Bad_Command_Line
              with
                "Zero or one argument expected";
      end case;
   end Data_Directory;

   procedure Define_Builtins (Interpreter : in out Interpreters.Interpreter_Type)
   is
      use Engine_Values.Handlers;

      Fibonacci : constant Integer_Array (1 .. 5) := (1, 1, 2, 3, 5);
   begin
      Interpreters.Define_Procedure
        (Interpreter => Interpreter,
         Name        => "success",
         Proc        =>
           Callback_Procedure_Handler'(Create (Val            => Test_Results.Success'Access,
                                               Min_Parameters => 0,
                                               Max_Parameters => 2)));
      Interpreters.Define_Procedure
        (Interpreter => Interpreter,
         Name        => "failure",
         Proc        =>
           Callback_Procedure_Handler'(Create (Val            => Test_Results.Failure'Access,
                                               Min_Parameters => 0,
                                               Max_Parameters => 1)));

      Interpreters.Define
        (Interpreter => Interpreter,
         Name        => "fibo",
         Value       => Integer_Array_Handlers.Create (Fibonacci),
         Read_Only   => True);
   end Define_Builtins;


   procedure Run_Test (Test_Source : Directories.Directory_Entry_Type)
   is
      function To_Target (Source : String) return String
      is
         Name   : constant String := Directories.Base_Name (Source);
         Target : constant String :=
                    Directories.Compose (Containing_Directory => "/tmp",
                                         Name                 => Name,
                                         Extension            => "out");
      begin
         Put_Line ("[" & Target & "]");
         return Target;
      end To_Target;

      Filename : constant String := Directories.Full_Name (Test_Source);

      Consumer : constant Consumers.Consumer_Access :=
                   Consumers.File_Writer.Open (To_Target (Filename));

      Interpreter : Interpreters.Interpreter_Type;
   begin
      Define_Builtins (Interpreter);
      Test_Results.New_Suite (Directories.Base_Name (Filename));

      Interpreters.Run (Interpreter => Interpreter,
                        Program     => Interpreters.Slurp (Filename),
                        Consumer    => Consumer);
   end Run_Test;

begin
   Directories.Search (Directory => Data_Directory,
                       Pattern   => "*.tmpl",
                       Filter    => (Directories.Ordinary_File => True,
                                     others                    => False),
                       Process => Run_Test'Access);

   Test_Results.Print_Report;
end Multi_Test.Main;
