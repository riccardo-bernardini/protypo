with Ada.Text_IO; use Ada.Text_IO;
with Readable_Sequences.String_Sequences;

procedure String_Sequences_Test is
   use Readable_Sequences.String_Sequences;

   S : Sequence := Empty_Sequence;
   A : constant String := "pippo pluto e paperino";
   b : constant string := " aldo giovanni e giacomo";
begin
   S.Append (A);
   S.Append (B);

   Put_Line (Boolean'Image (S.Length = A'Length + B'Length));
   Put_Line (S.Dump);
end String_Sequences_Test;
