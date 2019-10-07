with Ada.Finalization;
with Ada.Text_IO;

package Protypo.API.Consumers.File_Writer is
   type Target_Name(<>) is private;

   function To_Target (X : String) return Target_Name;

   Standard_Output : constant Target_Name;
   Standard_Error  : constant Target_Name;


   type Writer (<>) is
     new Ada.Finalization.Limited_Controlled and Consumer_Interface
   with private;

   function Open (target : Target_Name) return Consumer_Access;

   overriding
   procedure Process (Consumer  : in out Writer;
                      Parameter : String);

   procedure Close (Consumer : in out Writer);

private
   type Target_class is (Stderr, Stdout, File);

   type Target_Name (Class : Target_class; Length : Natural) is
      record
         case Class is
            when Stderr | Stdout =>
               null;

            when File =>
               Name : String (1 .. Length);
         end case;
      end record;

   function To_Target (X : String) return Target_Name
   is (Target_Name'(Class  => file,
                    Length => X'Length,
                    Name   => X));

   Standard_Output : constant Target_Name := Target_Name'(Class => Stdout, Length => 0);
   Standard_Error  : constant Target_Name := Target_Name'(Class => Stderr, Length => 0);


   type Writer is
     new Ada.Finalization.Limited_Controlled and Consumer_Interface
   with
      record
         Target : Target_class;
         Open   : Boolean;
         Output : Ada.Text_IO.File_Type;
      end record;

   overriding procedure Finalize (Obj : in out Writer);
end Protypo.API.Consumers.File_Writer;
