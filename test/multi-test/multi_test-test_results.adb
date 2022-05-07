pragma Ada_2012;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Text_Io; use Ada.Text_Io;

with Multi_Test.Tty_Colors;

package body Multi_Test.Test_Results is

   type Test_Result is
      record
         Success : Boolean;
         Message : Unbounded_String;
         Label   : Unbounded_String;
      end record;

   package Test_Result_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Test_Result);

   type Suite_Result is
      record
         Label   : Unbounded_String;
         Results : Test_Result_Lists.List;
         Success : Boolean;
      end record;

   package Suite_Result_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Suite_Result);

   Result_Journal : Suite_Result_Lists.List;


   function Test_Number return Positive
   is (Natural (Result_Journal.Last_Element.Results.Length) + 1);

   function Current_Test_Label return String
   is ("Test N. " & Test_Number'Image);

   ---------------
   -- New_Suite --
   ---------------

   procedure New_Suite (Label : String) is
   begin
      Result_Journal.Append
        (Suite_Result'(Label   => To_Unbounded_String (Label),
                       Results => Test_Result_Lists.Empty_List,
                       Success => True));
   end New_Suite;

   procedure Register_Result (Success : Boolean;
                              Message : String;
                              Label   : String)
   is
   begin
      if Result_Journal.Is_Empty then
         raise Program_Error;
      end if;

      declare
         Current_Suite : constant Suite_Result_Lists.Reference_Type :=
                           Result_Journal.Reference (Result_Journal.Last);
      begin
         Current_Suite.Results.Append
           (Test_Result'(Success => Success,
                         Message => To_Unbounded_String (Message),
                         Label   => To_Unbounded_String (Label)));

         Current_Suite.Success := Current_Suite.Success and Success;
      end;
   end Register_Result;

   -------------
   -- Success --
   -------------

   procedure Success (Params : Protypo.Api.Engine_Values.Engine_Value_Array) is
      use Protypo.Api.Engine_Values;

      First : constant Engine_Value := Params (Engine_Index'First);

      Second : constant Engine_Value := Params (Engine_Index'First + 1);
   begin
      if First.Class = Void and Second.Class = Void then
         Register_Result (Success => True,
                          Message => "",
                          Label   => Current_Test_Label);

      elsif First.Class = Int and Second.Class = Void then
         Register_Result (Success => Get_Boolean (Params.First_Element),
                          Message => "",
                          Label   => Current_Test_Label);

      elsif First.Class = Int and Second.Class = Text then
         Register_Result (Success => Get_Boolean (First),
                          Message => Get_String (Second),
                          Label   => Current_Test_Label);
      else
         raise Constraint_Error
           with "Bad parameter combination: "
           & First.Class'Image
           & ", " & Second.Class'Image;

      end if;
   end Success;

   -------------
   -- Failure --
   -------------

   procedure Failure (Params : Protypo.Api.Engine_Values.Engine_Value_Array) is
      use Protypo.Api.Engine_Values;

      First : constant Engine_Value := Params (Engine_Index'First);
   begin
      case First.Class is
         when Void =>
            Register_Result (Success => False,
                             Message => "",
                             Label   => Current_Test_Label);

         when Text =>
            Register_Result (Success => False,
                             Message => Get_String (Params.First_Element),
                             Label   => Current_Test_Label);

         when others =>
            raise Constraint_Error
              with "Expected string, found " & Params.First_Element.Class'Image;

      end case;
   end Failure;

   ------------------
   -- Print_Report --
   ------------------

   procedure Print_Report is
      use Tty_Colors;

      function Format_Success (Success : Boolean) return String
      is
      begin
         if Success then
            return Color_Text ("SUCCESS", Green);

         else
            return Color_Text ("SUCCESS", Green);
         end if;
      end Format_Success;

      function Format_Suite_Name (Name : String) return String
      is ("Suite " & Color_Text (Name, Blue));

      function Format_Test_Label (Label : String) return String
      is (Label);

      procedure Print_Suite_Report (Suite : Suite_Result) is
      begin
         Put_Line (Format_Suite_Name (To_String (Suite.Label)));
         New_Line;

         for Test of Suite.Results loop
            Put_Line (Format_Test_Label (To_String (Test.Label))
                      & " "
                      & Format_Success (Test.Success)
                      & " "
                      & To_String (Test.Message));
         end loop;

         New_Line;
      end Print_Suite_Report;

      Overall_Success : Boolean := True;
      Suite_Name_Max_Length : Natural := 0;
   begin
      for Suite of Result_Journal loop
         Print_Suite_Report (Suite);

         Suite_Name_Max_Length :=
           Natural'Max (Suite_Name_Max_Length, Length (Suite.Label));

         Overall_Success := Overall_Success and Suite.Success;
      end loop;

      Put_Line ("Summary:");
      New_Line;

      declare
         use Ada.Strings;

         Padded_Name : String (1 .. Suite_Name_Max_Length+3);
      begin
         for Suite of Result_Journal loop
            Fixed.Move (Source  => To_String (Suite.Label) & " ",
                        Target  => Padded_Name,
                        Justify => Left,
                        Pad     => '.');

            Put_Line (Padded_Name
                      & " "
                      & Format_Success (Suite.Success));
         end loop;

         New_Line;

         Put_Line (Fixed."*" (Padded_Name'Length, '-'));
         Put_Line ("Overall : " & Format_Success (Overall_Success));
      end;

      New_Line;
   end Print_Report;

end Multi_Test.Test_Results;
