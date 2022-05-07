pragma Ada_2012;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

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
   is (Positive (Result_Journal.Last_Element.Results.Length) + 1);


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
      use type Ada.Containers.Count_Type;
      use Protypo.Api.Engine_Values;

   begin
      case Params.Length is
         when 0 =>
            Register_Result (Success => True,
                             Message => "",
                             Label   => Test_Number'Image);

         when 1 =>
            if  Params.First_Element.Class /= Int then
               raise Constraint_Error
                 with "Expected int, found " & Params.First_Element.Class'Image;
            end if;

            Register_Result (Success => Get_Boolean (Params.First_Element),
                             Message => "",
                             Label   => Test_Number'Image);


         when 2 =>
            declare
               First : constant Engine_Value :=
                         Params (Engine_Index'First);

               Second : constant Engine_Value :=
                          Params (Engine_Index'First + 1);

            begin
               if First.Class /= Int or else Second.Class /= Text then
                  raise Constraint_Error
                    with "Expected int+string";
               end if;

               Register_Result (Success => Get_Boolean (First),
                                Message => Get_String (Second),
                                Label   => Test_Number'Image);
            end;

         when others =>
            raise Constraint_Error;

      end case;
   end Success;

   -------------
   -- Failure --
   -------------

   procedure Failure (Params : Protypo.Api.Engine_Values.Engine_Value_Array) is
      use type Ada.Containers.Count_Type;
      use Protypo.Api.Engine_Values;
   begin
      case Params.Length is
         when 0 =>
            Register_Result (Success => False,
                             Message => "",
                             Label   => Test_Number'Image);

         when 1 =>
            if  Params.First_Element.Class /= Text then
               raise Constraint_Error
                 with "Expected string, found " & Params.First_Element.Class'Image;
            end if;

            Register_Result (Success => False,
                             Message => Get_String (Params.First_Element),
                             Label   => Test_Number'Image);

         when others =>
            raise Constraint_Error;

      end case;
   end Failure;

   ------------------
   -- Print_Report --
   ------------------

   procedure Print_Report is
      procedure Print_Suite_Report (Suite : Suite_Result) is
      begin
         Put_Line ("[" & To_String (Suite.Label) & "]");
         New_Line;

         for Test of Suite.Results loop
            Put_Line (To_String (Test.Label)
                      & " "
                      & (if Test.Success then "SUCCESS" else "FAIL")
                      & " "
                      & To_String (Test.Message));
         end loop;

         New_Line;
         Put_Line (" ------ ");
         New_Line;
      end Print_Suite_Report;

      Overall_Success : Boolean := True;
   begin
      for Suite of Result_Journal loop
         Print_Suite_Report (Suite);

         Overall_Success := Overall_Success and Suite.Success;
      end loop;

      Put_Line ("Summary:");
      New_Line;
      for Suite of Result_Journal loop
         Put_Line (To_String (Suite.Label)
                   & " "
                   & (if Suite.Success then "success" else "FAIL"));
      end loop;

      Put_Line ("Overall : "
                & (if Overall_Success then "SUCCESS" else "FAIL"));

      New_Line;
   end Print_Report;

end Multi_Test.Test_Results;
