with Ada.Finalization;
with Ada.Text_IO;

package Protypo.API.Consumers.File_Writer is
   type Writer (<>) is
     new Ada.Finalization.Limited_Controlled and Consumer_Interface
   with private;

   Standard_Output : constant String;
   Standard_Error  : constant String;

   function Open (Filename : String) return Consumer_Access;

   overriding
   procedure Process (Consumer  : in out Writer;
                      Parameter : String);

   procedure Close (Consumer : in out Writer);

private
   Standard_Output : constant String := (1 => Character'Val (0));
   Standard_Error  : constant String := (1 .. 2 => Character'Val (0));

   type Target_Type is (Stderr, Stdout, File);

   type Writer is
     new Ada.Finalization.Limited_Controlled and Consumer_Interface
   with
      record
         Target : Target_Type;
         Open   : Boolean;
         Output :  Ada.Text_IO.File_Type;
      end record;

   overriding procedure Finalize (Obj : in out Writer);
end Protypo.API.Consumers.File_Writer;
