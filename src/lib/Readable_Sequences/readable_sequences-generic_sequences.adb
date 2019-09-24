pragma Ada_2012;
package body Readable_Sequences.Generic_Sequences is


   ----------
   -- Next --
   ----------

   function Next (Seq : in out Sequence) return Element_Type
   is
      Result : constant Element_Type := Seq.Read;
   begin
      Seq.Next;
      return Result;
   end Next;

   ----------
   -- Dump --
   ----------

   function Dump (Seq : Sequence) return Element_Array
   is
      Result : Element_Array (Integer (Seq.Vector.First_Index) .. Integer (Seq.Vector.Last_Index));
   begin
      for K in Result'Range loop
         Result (K) := Seq.Vector (Cursor (K));
      end loop;

      return Result;
   end Dump;

   -----------
   -- Clear --
   -----------

   procedure Clear (Seq : in out Sequence)
   is
   begin
      Seq.Vector.Clear;
      Seq.Position := Beginning;
      Seq.Position_Saved := False;
   end Clear;

   ------------
   -- Create --
   ------------

   function Create (End_Of_Sequence_Marker : Element_Type) return Sequence
   is
   begin
      return Sequence'(Vector         => Element_Vectors.Empty_Vector,
                       Position       => Cursor'First,
                       Old_Position   => <>,
                       Position_Saved => False,
                       Has_End_Marker => True,
                       End_Marker     => End_Of_Sequence_Marker);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Init                   : Element_Array;
                    End_Of_Sequence_Marker : Element_Type) return Sequence
   is
      Result : Sequence := Sequence'(Vector         => Element_Vectors.Empty_Vector,
                                     Position       => Cursor'First,
                                     Old_Position   => <>,
                                     Position_Saved => False,
                                     Has_End_Marker => True,
                                     End_Marker     => End_Of_Sequence_Marker);
   begin
      Result.Append (Init);

      return Result;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Init : Element_Array) return Sequence
   is
      Result : Sequence := Sequence'(Vector         => Element_Vectors.Empty_Vector,
                                     Position       => Cursor'First,
                                     Old_Position   => <>,
                                     Position_Saved => False,
                                     Has_End_Marker => False,
                                     End_Marker     => <>);
   begin
      Result.Append (Init);

      return Result;
   end Create;




   ------------
   -- Append --
   ------------

   procedure Append
     (Seq      : in out Sequence;
      Elements : Element_Array)
   is
   begin
      for C of Elements loop
         Seq.Append (C);
      end loop;
   end Append;

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
   -- Append --
   ------------

   procedure Append (To   : in out Sequence;
                     What : Sequence)
   is
   begin
      To.Vector.Append (What.Vector);
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

   procedure Next (Seq  : in out Sequence;
                   Step : Positive := 1)
   is
   begin
      if Seq.Remaining < Step then
         Seq.Position := Seq.Vector.Last_Index + 1;
         return;
      end if;

      Seq.Position := Seq.Position + Cursor (Step);
   end Next;


   ----------
   -- Back --
   ----------

   procedure Back (Seq  : in out Sequence;
                   Step : Positive := 1)
   is
   begin
      if Seq.Position < Seq.Vector.First_Index + Cursor (Step) then
         Seq.Position := Seq.Vector.First_Index;
         return;
      end if;

      Seq.Position := Seq.Position - Cursor (Step);
   end Back;


   -------------
   -- Process --
   -------------

   procedure Process (Seq      : Sequence;
                      Callback : access procedure (Item : Element_Type))
   is
   begin
      for El of Seq.Vector loop
         Callback (El);
      end loop;
   end Process;

end Readable_Sequences.Generic_Sequences;
