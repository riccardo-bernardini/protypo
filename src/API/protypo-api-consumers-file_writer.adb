pragma Ada_2012;
package body Protypo.API.Consumers.File_Writer is

   ----------
   -- Open --
   ----------

   function Open (Filename : String) return Writer is
   begin
      return Result : Writer do
         Ada.Text_IO.Create (File => Result.output,
                             Mode => Ada.Text_IO.Out_File,
                             Name => Filename);
      end return;
   end Open;

   -------------
   -- Process --
   -------------

   overriding procedure Process
     (Consumer  : in out Writer;
      Parameter : String)
   is
   begin
      Ada.Text_IO.Put (Consumer.Output, Parameter);
   end Process;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Obj : in out Writer)
   is
   begin
      Ada.Text_IO.Close (Obj.Output);
   end Finalize;

end Protypo.API.Consumers.File_Writer;
