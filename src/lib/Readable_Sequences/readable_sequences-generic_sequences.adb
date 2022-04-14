pragma Ada_2012;

pragma Warnings (Off, "no entities of ""Ada.Text_Io"" are referenced");
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Unchecked_Deallocation;
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

      return Element_Array (Seq.Buffer (From .. Seq.Buffer'Last));
   end Dump;

   -----------
   -- Clear --
   -----------

   procedure Clear (Seq : in out Sequence)
   is
   begin
      Seq.Position := Seq.Buffer'First;
      Seq.First_Free := Seq.Buffer'First;
      Seq.Position_Saved := False;
   end Clear;

   function Allocate_Buffer (Min_Size : Natural) return Buffer_Access
   is
      Blocksize : constant Positive := 2048;
      N_Blocks  : constant Positive := Min_Size / Blocksize + 2;
   begin
      --
      -- Blocksize * (min_size/blocksize) >= min_size - Blocksize
      --
      -- Blocksize * N_blocks >= min_size + Blocksize
      --
      -- We get at least a full block free
      --
      pragma Assert (Blocksize * N_Blocks >= Min_Size + Blocksize);

      return new Buffer_Type (1 .. Cursor (Blocksize * N_Blocks));
   end Allocate_Buffer;

   function Empty_Sequence return Sequence
   is
      Tmp : constant Buffer_Access := Allocate_Buffer (0);
   begin
      return (Sequence'(Ada.Finalization.Limited_Controlled
              with
                Buffer         => Tmp,
              Position       => Tmp'First,
              First_Free     => Tmp'First,
              Old_Position   => <>,
              Position_Saved => False,
              Has_End_Marker => False,
              End_Marker     => <>));
   end Empty_Sequence;


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

   --  procedure Update (Seq  : in out Sequence;
   --                    Data : Buffer_Type)
   --  is
   --  begin
   --     Seq.Buffer.Replace_Element (Data);
   --     Seq.First := Data'First;
   --     Seq.After_Last  := Data'Last + 1;
   --     Seq.Position := Seq.First;
   --  end Update;


   ------------
   -- Append --
   ------------

   procedure Append
     (Seq      : in out Sequence;
      Elements : Element_Array)
   is
   begin
      if Seq.Free_Space >= Elements'Length then

         declare
            Last : constant Cursor := Seq.First_Free + Cursor (Elements'Length);
         begin
            pragma Assert (Last <= Seq.Buffer'Last);

            Seq.Buffer (Seq.First_Free .. Last) := Buffer_Type (Elements);
            Seq.First_Free := Last + 1;
         end;

      else

         declare
            type Nullable_Buffer_Access is access all Buffer_Type;

            procedure Free is
              new Ada.Unchecked_Deallocation (Object => Buffer_Type,
                                              Name   => Nullable_Buffer_Access);

            Old_Buffer : Nullable_Buffer_Access :=
                           Nullable_Buffer_Access (Seq.Buffer);

            --
            --  Why this?  Because Buffer_Access has been declared "not null"
            --  but Unchecked_Deallocation set to null its paramete.
            --

            New_Length : constant Natural := Seq.Length + Elements'Length;
            New_Buffer : constant Buffer_Access := Allocate_Buffer (New_Length);
            Last_Written : constant Cursor :=
                             New_Buffer'First + Cursor (Elements'Length) - 1;
         begin
            New_Buffer (New_Buffer'First .. Last_Written) :=
              Buffer_Type (Seq.Dump & Elements);

            Seq.Buffer := New_Buffer;

            Seq.First_Free := Last_Written + 1;

            Free (Old_Buffer);
         end;

      end if;
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
      To.Append (Element_Array (What.Buffer.all));
   end Append;


   function Remaining (Seq : Sequence) return Natural
   is
   begin
      --  Put_Line (Seq.Buffer'Last'Image);
      --  Put_Line (Seq.Position'Image);
      --  Put_Line (Cursor'(Seq.Buffer'Last - Seq.Position + 1 )'Image);
      --  Put_Line (Seq.Length'Image);

      return Integer (Seq.First_Free) - Integer (Seq.Position) + 1;
   end Remaining;

   ------------
   -- Rewind --
   ------------

   procedure Rewind
     (Seq : in out Sequence;
      To  :        Cursor := Beginning)
   is
   begin
      if To > Seq.First_Free then
         raise Constraint_Error;
      end if;

      if To = Beginning then
         Seq.Position := Seq.Buffer'First;
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
         Seq.Position := Seq.First_Free;
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
      if Seq.Position < Seq.Buffer'First + Cursor (Step) then
         Seq.Position := Seq.Buffer'First;
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
      for El of Seq.Buffer.all loop
         Callback (El);
      end loop;
   end Process;

end Readable_Sequences.Generic_Sequences;
