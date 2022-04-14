with Ada.Finalization;

--
--  The model for a "readable sequence" is a sequential data buffer where
--  new data can be only appended and data can be read sequentially,
--  although the "cursor" to the current entry can be moved (mainly
--  by saving the current position and restoring it)
--

generic
   type Element_Type is private;
   type Element_Array is array (Positive range <>) of Element_Type;
package Readable_Sequences.Generic_Sequences is
   type Sequence is
     new Ada.Finalization.Limited_Controlled
   with
     private;


   type Cursor is private;


   function Empty_Sequence return Sequence;

   function Create (Init : Element_Array) return Sequence;

   function Create (End_Of_Sequence_Marker : Element_Type) return Sequence;

   function Create (Init                   : Element_Array;
                    End_Of_Sequence_Marker : Element_Type) return Sequence;

   function Has_End_Of_Sequence_Marker (Item : Sequence) return Boolean;

   function Dump (Seq  : Sequence) return Element_Array
     with
       Post => Dump'Result'Length = Seq.Length;

   function Dump (Seq  : Sequence;
                  From : Cursor) return Element_Array;

   function First (Seq : Sequence) return Cursor
     with
       Post => Seq.Is_Valid_Position (First'Result);

   procedure Clear (Seq : in out Sequence)
     with
       Post =>
         Seq.Length = 0
         and not Seq.Saved_Position
         and Seq.Current_Position = Seq.First;

   function Index (Seq : Sequence) return Positive;

   procedure Append (Seq      : in out Sequence;
                     Elements : Element_Array)
     with
       Post =>
         Seq.Length = Seq.Length'Old + Elements'Length
         and Seq.Remaining = Seq.Remaining'Old + Elements'Length;

   procedure Append (To   : in out Sequence;
                     What : Element_Type)
     with
       Post =>
         To.Length = To.Length'Old + 1
         and to.Remaining = to.Remaining'Old + 1;

   procedure Append (To   : in out Sequence;
                     What : Sequence)
     with
       Post =>
         To.Length = To.Length'Old + What.Length
         and to.Remaining = to.Remaining'Old + What.Length;

   procedure Rewind (Seq : in out Sequence)
     with
       Post =>
         Seq.Current_Position = Seq.First
         and Seq.Remaining = Seq.Length;

   procedure Rewind (Seq : in out Sequence;
                     To  :        Cursor)
     with
       Pre =>
         Seq.Is_Valid_Position (To),

         Post =>
           Seq.Current_Position = To;

   function Length (Seq : Sequence) return Natural;
   --  Total number of data written in the buffer

   function Remaining (Seq : Sequence) return Natural
     with Post => Remaining'Result <= Seq.Length;
   -- Return the number of elements that still need to be read from
   -- Seq.  This includes also the current element

   function Is_Valid_Position (Seq : Sequence;
                               C   : Cursor)
                               return Boolean;

   function Current_Position (Seq : Sequence) return Cursor
     with
       Post => Seq.Is_Valid_Position (Current_Position'Result);

   function Next_Position (Seq : Sequence) return Cursor
     with
       Post => Seq.Is_Valid_Position (Next_Position'Result);
   --  Return the position will have the reading cursor after a call to Next.
   --  Funny?  It is useful for contracts


   procedure Set_Position (Seq : in out Sequence;
                           Pos : Cursor)
     with
       Pre =>
         Seq.Is_Valid_Position (Pos),

         Post =>
           Seq.Current_Position = Pos;


   function Saved_Position (Seq : Sequence) return Boolean;

   procedure Save_Position (Seq : in out Sequence)
     with
       Pre => not Seq.Saved_Position,
       Post => Seq.Saved_Position;

   procedure Restore_Position (Seq : in out Sequence)
     with
       Pre => Seq.Saved_Position,
       Post => not Seq.Saved_Position;

   procedure Clear_Position (Seq : in out Sequence)
     with
       Pre => Seq.Saved_Position,
       Post => not Seq.Saved_Position;

   function End_Of_Sequence (Seq : Sequence) return Boolean
   is (Seq.Remaining = 0);

   function Read (Seq   : Sequence;
                  Ahead : Natural := 0) return Element_Type
     with
       Pre => Seq.Has_End_Of_Sequence_Marker or Seq.Remaining > Ahead;

   function Next (Seq : in out Sequence) return Element_Type
     with
       Pre =>
         Seq.Has_End_Of_Sequence_Marker or Seq.Remaining > 0,

       Post =>
         Next'Result = Seq.Read'Old
         and Seq.Current_Position = Seq.Next_Position'Old;


   procedure Next (Seq  : in out Sequence;
                   Step : Positive := 1)
     with
       Post => Seq.Remaining = Integer'Max (Seq.Remaining'Old - Step, 0);

   procedure Back (Seq  : in out Sequence;
                   Step : Positive := 1)
     with
       Post => Seq.Remaining = Integer'Min (Seq.Remaining'Old + Step, Seq.Length);

   procedure Process (Seq      : Sequence;
                      Callback : access procedure (Item : Element_Type));

   Beyond_End : exception;
private

   type Cursor is range 0 .. Integer'Last;

   type Buffer_Type is array (Cursor range <>) of Element_Type;

   type Buffer_Access is not null access Buffer_Type;


   function Free_Space (Seq : Sequence) return Natural;

   --
   -- A "Sequence" is basically a buffer with two cursors:
   --
   -- * One Cursor Points To The "current" Element That is Returned
   --   by "reading" functions
   --
   -- * The other cursor points to the first free location that it
   --   has not been initialized yet.  This cursor can point beyond
   --   the end of the buffer, if the buffer is filled.
   --
   -- "End of Sequence" condition happens when the two cursors are equal
   --
   type Sequence is
     new Ada.Finalization.Limited_Controlled
   with
      record
         Buffer         : Buffer_Access;
         First_Free     : Cursor;
         Position       : Cursor;
         Old_Position   : Cursor;
         Position_Saved : Boolean := False;
         Has_End_Marker : Boolean := False;
         End_Marker     : Element_Type;
      end record
     with
       Type_Invariant =>
         Sequence.First_Free <= Sequence.Buffer.all'Last + 1
         and then Sequence.Is_Valid_Position (Sequence.Position);

   overriding
   procedure Finalize (Object : in out Sequence);


   function Saved_Position (Seq : Sequence)return Boolean
   is (Seq.Position_Saved);


   function Current_Position (Seq : Sequence) return Cursor
   is (Seq.Position);

   function Length (Seq : Sequence) return Natural
   is (Natural (Seq.First_Free - Seq.Buffer'First));

   function Read (Seq   : Sequence;
                  Ahead : Natural := 0) return Element_Type
   is (if Seq.Remaining > Ahead then
          Seq.Buffer (Cursor (Integer (Seq.Position) + Ahead))

       elsif Seq.Has_End_Marker then
          Seq.End_Marker

       else
          raise Beyond_End);

   function Has_End_Of_Sequence_Marker (Item : Sequence) return Boolean
   is (Item.Has_End_Marker);

   function Index (Seq : Sequence) return Positive
   is (Positive (Seq.Position));

   function Is_Valid_Position (Seq : Sequence;
                               C   : Cursor)
                               return Boolean
   is (C >= Seq.Buffer'First and C <= Seq.First_Free);

   function Next_Position (Seq : Sequence) return Cursor
   is (if Seq.End_Of_Sequence then
          Seq.Position
       else
          Seq.Position + 1);


   function Free_Space (Seq : Sequence) return Natural
   is (Integer (Seq.Buffer'Last)-Integer (Seq.First_Free) + 1);

   function First (Seq : Sequence) return Cursor
   is (Seq.Buffer'First);

end Readable_Sequences.Generic_Sequences;
