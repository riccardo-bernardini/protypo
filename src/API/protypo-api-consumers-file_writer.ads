with Ada.Finalization;
with Ada.Text_Io;

--
-- ## What is this?
--
-- A _file writer_ implements the `Consumer_Interface` that just
-- writes the received strings to an external file (that can also be
-- standard output or standard error).
--
-- A writer needs to be associated to an external file when it
-- is created.  This can be done with the function `Open`. Special
-- constant to write to standard output/error are provided.
--
package Protypo.Api.Consumers.File_Writer is
   type Writer (<>) is
     new Ada.Finalization.Limited_Controlled and Consumer_Interface
   with private;


   overriding procedure Process (Consumer  : in out Writer;
                                 Parameter : String);



   Standard_Output_Special_Name : constant String;
   Standard_Error_Special_Name  : constant String;

   function Open (Target : String) return Consumer_Access;

   procedure Close (Consumer : in out Writer);
private
   type Target_Class is (Stderr, Stdout, File);

   type Target_Name (Class : Target_Class; Length : Natural) is
      record
         case Class is
            when Stderr | Stdout =>
               null;

            when File =>
               Name : String (1 .. Length);
         end case;
      end record;

   function Open (Target : Target_Name) return Consumer_Access;

   Standard_Output_Special_Name : constant String := ".";
   Standard_Error_Special_Name  : constant String := "..";


   function To_Target (X : String) return Target_Name
   is (if X = Standard_Output_Special_Name then
          Target_Name'(Class =>  Stdout, Length => 0)
       elsif X = Standard_Error_Special_Name then
          Target_Name'(Class =>  Stderr, Length => 0)
       else
          Target_Name'(Class  => File,
                       Length => X'Length,
                       Name   => X));

   function Open (Target : String) return Consumer_Access
   is (Open (To_Target (Target)));


   type Writer is
     new Ada.Finalization.Limited_Controlled and Consumer_Interface
   with
      record
         Target : Target_Class;
         Open   : Boolean;
         Output : Ada.Text_Io.File_Type;
      end record;

   overriding procedure Finalize (Obj : in out Writer);
end Protypo.Api.Consumers.File_Writer;
