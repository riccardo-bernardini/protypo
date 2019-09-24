with Ada.Finalization;
with Ada.Text_IO;

package Protypo.API.Consumers.File_Writer is
   type Writer is
     new Ada.Finalization.Limited_Controlled and Consumer_Interface
   with private;

   function Open (Filename : String) return Writer;

   overriding
   procedure Process (Consumer  : in out Writer;
                      Parameter : String);

private
   type Writer is
     new Ada.Finalization.Limited_Controlled and Consumer_Interface
     with
      record
         Output : Ada.Text_IO.File_Type;
      end record;

   overriding procedure Finalize (Obj : in out Writer);
end Protypo.API.Consumers.File_Writer;
