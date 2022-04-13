pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Readable_Sequences.Generic_Sequences is

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Seq : in out Sequence;
                         Pos : Cursor)
   is
   begin
      Seq.Position := Pos;
   end Set_Position;

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

   function Dump (Seq  : Sequence;
                  From : Cursor := Beginning) return Element_Array
   is
      --  Result : Element_Array (Integer (From) .. Integer (Seq.Buffer'Last));
   begin
      --  for K in Result'Range loop
      --     Result (K) := Seq.Buffer (Cursor (K));
      --  end loop;

      return Element_Array (Seq.Buffer.Element (From .. Seq.Buffer.Element'Last));
   end Dump;

   -----------
   -- Clear --
   -----------

   procedure Clear (Seq : in out Sequence)
   is
   begin
      Seq.Position := Beginning;
      Seq.Position_Saved := False;
   end Clear;

   ------------
   -- Create --
   ------------

   function Create (End_Of_Sequence_Marker : Element_Type) return Sequence
   is

   begin
      return Result : Sequence := Empty_Sequence do
         Result.End_Marker := End_Of_Sequence_Marker;
         Result.Has_End_Marker := True;
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Init                   : Element_Array;
                    End_Of_Sequence_Marker : Element_Type) return Sequence
   is

   begin
      return Result : Sequence := Create (End_Of_Sequence_Marker) do
         Result.Append (Init);
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Init : Element_Array) return Sequence
   is

   begin
      return Result : Sequence := Empty_Sequence do
         Result.Append (Init);
      end return;
   end Create;

   procedure Update (Seq : in out Sequence;
                     Data : Buffer_Type)
   is
   begin
      Seq.Buffer.Replace_Element (Data);
      Seq.First := Data'First;
      Seq.After_Last  := Data'Last + 1;
      Seq.Position := Seq.First;
   end Update;


   ------------
   -- Append --
   ------------

   procedure Append
     (Seq      : in out Sequence;
      Elements : Element_Array)
   is
      New_Buffer : constant Buffer_Type :=
                     Seq.Buffer.Element (Seq.First .. Seq.After_Last - 1)
                   & Buffer_Type (Elements);
   begin
      Update (Seq, New_Buffer);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (To   : in out Sequence;
      What : Element_Type)
   is
   begin
      To.Append (Element_Array'(1 => What));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (To   : in out Sequence;
                     What : Sequence)
   is
   begin
      To.Append (Element_Array (What.Buffer.Element));
   end Append;


   function Remaining (Seq : Sequence) return Natural
   is
   begin
      Put_Line (Seq.Buffer.Element'Last'Image);
      Put_Line (Seq.Position'image);
      Put_Line (cursor'(Seq.Buffer.Element'Last - Seq.Position+1 )'Image);
      Put_Line (Seq.Length'Image);

      return Integer (Seq.Buffer.Element'Last - Seq.Position + 1);
   end Remaining;

   ------------
   -- Rewind --
   ------------

   procedure Rewind
     (Seq : in out Sequence;
      To  :        Cursor := Beginning)
   is
   begin
      if To >= Seq.After_Last then
         raise Constraint_Error;
      end if;

      if To = Beginning then
         Seq.Position := Seq.First;
      else
         Seq.Position := To;
      end if;
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
         Seq.Position := Seq.After_Last;
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
      if Seq.Position < Seq.First + Cursor (Step) then
         Seq.Position := Seq.First;
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
      for El of Seq.Buffer.Element loop
         Callback (El);
      end loop;
   end Process;

end Readable_Sequences.Generic_Sequences;
