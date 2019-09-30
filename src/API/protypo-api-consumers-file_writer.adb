pragma Ada_2012;
package body Protypo.API.Consumers.File_Writer is

   ----------
   -- Open --
   ----------

   function Open (Filename : String) return Consumer_Access is
      use Ada.Finalization;

      type Writer_Access is access Writer;

      Target : constant Target_Type := (if Filename = Standard_Output then
                                           Stdout

                                        elsif Filename = Standard_Error then
                                           Stderr

                                        else
                                           File);

      Result : constant Writer_Access := new Writer'(Limited_Controlled with
                                                     Target        => Target,
                                                     Open          => True,
                                                     Output        => <>);
   begin
      if Result.Target = File then
         Ada.Text_IO.Create (File => Result.Output,
                             Mode => Ada.Text_IO.Out_File,
                             Name => Filename);
      end if;

      return Result;
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

   -----------
   -- Close --
   -----------

   procedure Close (Consumer : in out Writer)
   is
   begin
      if Consumer.Open and Consumer.Target = File then
         Ada.Text_IO.Close (Consumer.Output);
         Consumer.Open := False;
      end if;
   end Close;


   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Obj : in out Writer)
   is
   begin
      Close (Obj);
   end Finalize;

end Protypo.API.Consumers.File_Writer;
