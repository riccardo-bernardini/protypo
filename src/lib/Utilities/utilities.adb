with Ada.Sequential_IO;
with Ada.Directories;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
pragma Warnings (Off, "use clause for package ""Text_IO"" has no effect");
with Ada.Text_IO; use Ada.Text_IO;

package body Utilities is


   -----------
   -- Slurp --
   -----------

   function Slurp (Filename : String) return String is

      subtype Content is String (1 .. Integer (Ada.Directories.Size (Filename)));
      Result : Content;

      package Content_IO is new Ada.Sequential_IO (Content);
      Input  : Content_IO.File_Type;

   begin
      --  Put_Line (Filename'Length'Image);
      --  Put_Line (Filename);

      Content_IO.Open (File     => Input,
                       Mode     => Content_IO.In_File,
                       Name     => Filename);

      Content_IO.Read (File => Input,
                       Item => Result);

      Content_IO.Close (Input);

      return Result;
   end Slurp;
end Utilities;
