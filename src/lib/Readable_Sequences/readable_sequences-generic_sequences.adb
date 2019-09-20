pragma Ada_2012;
package body Readable_Sequences.Generic_Sequences is

   ------------
   -- Append --
   ------------

   procedure Append
     (To   : in out Sequence;
      What : Element_Type)
   is
   begin
      To.Vector.Append (What);
   end Append;

   ------------
   -- Rewind --
   ------------

   procedure Rewind
     (Seq : in out Sequence;
      To  :        Cursor := Beginning)
   is
   begin
      if To > Seq.Vector.Last_Index then
         raise Constraint_Error;
      end if;

      Seq.Position := To;
   end Rewind;

   -------------------
   -- Save_Position --
   -------------------

   procedure Save_Position
     (Seq : in out Sequence)
   is
   begin
      if Seq.Position_Saved then
         raise Constraint_Error;
      end if;

      Seq.Old_Position := Seq.Position;
      Seq.Position_Saved := True;
   end Save_Position;

   ----------------------
   -- Restore_Position --
   ----------------------

   procedure Restore_Position
     (Seq : in out Sequence)
   is
   begin
      if not Seq.Position_Saved then
         raise Constraint_Error;
      end if;

      Seq.Position := Seq.Old_Position;
      Seq.Position_Saved := False;
   end Restore_Position;

   --------------------
   -- Clear_Position --
   --------------------

   procedure Clear_Position
     (Seq : in out Sequence)
   is
   begin
   if not Seq.Position_Saved then
         raise Constraint_Error;
      end if;

      Seq.Position_Saved := False;
   end Clear_Position;

   ----------
   -- Next --
   ----------

   procedure Next (Seq : in out Sequence) is
   begin
      if Seq.End_Of_Sequence then
         raise Constraint_Error;
      end if;

      Seq.Position := Seq.Position + 1;
   end Next;

end Readable_Sequences.Generic_Sequences;
