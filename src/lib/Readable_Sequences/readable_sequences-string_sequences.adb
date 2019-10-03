with Ada.Characters.Latin_9;

package body Readable_Sequences.String_Sequences is

   procedure Fill_Position (Seq : in out Sequence)
   is
      use Ada.Characters.Latin_9;

      Old_Position : constant Cursor := Seq.Current_Position;

      Current      : Position_Type := Position_Type'(Line => 1,
                                                     Char => 1);
   begin
      Seq.Rewind;

      while not Seq.End_Of_Sequence loop
         Seq.Position_Cache.Append (Current);

         if Seq.Read in CR | LF then
            Current := Position_Type'(Line => Current.Line+1,
                                      Char => 1);
         else
            Current.Char := Current.Char + 1;
         end if;

         Seq.Next;
      end loop;

      Seq.Set_Position (Old_Position);
   end Fill_Position;


   overriding
   function Create (Init : String) return Sequence
   is
   begin
      return Result : Sequence :=
        Sequence'
          (Basic_String_Sequences.Sequence'
             (Basic_String_Sequences.Create (Init))
           with
             Position_Cache => Position_Vectors.Empty_Vector)
      do
         Fill_Position (Result);
      end return;
   end Create;

   overriding
   function Create (End_Of_Sequence_Marker : Character) return Sequence
   is
   begin
      return Sequence'
        (Basic_String_Sequences.Sequence'
           (Basic_String_Sequences.Create (End_Of_Sequence_Marker))
         with
           Position_Cache => Position_Vectors.Empty_Vector);
   end Create;

   overriding
   function Create (Init                   : String;
                    End_Of_Sequence_Marker : Character) return Sequence
   is
   begin
      return Result : Sequence :=
        Sequence'(Basic_String_Sequences.Sequence'
                    (Basic_String_Sequences.Create (Init, End_Of_Sequence_Marker))
                  with
                    Position_Cache => Position_Vectors.Empty_Vector)
      do
         Fill_Position (Result);
      end return;
   end Create;



   function Position (Item : in out Sequence) return Position_Type
   is
   begin
      if Item.Index > Item.Position_Cache.Last_Index then
         Fill_Position (Item);
      end if;

      return Item.Position_Cache (Item.Index);
   end Position;
end Readable_Sequences.String_Sequences;
