with Ada.Containers.Indefinite_Holders;

generic
   type Element_Type is private;
   type Element_Array is array (Positive range <>) of Element_Type;
package Readable_Sequences.Generic_Sequences is
   type Sequence is tagged private;


   type Cursor is private;

   Beginning : constant Cursor;

   function Empty_Sequence return Sequence;

   function Create (Init : Element_Array) return Sequence;

   function Create (End_Of_Sequence_Marker : Element_Type) return Sequence;

   function Create (Init                   : Element_Array;
                    End_Of_Sequence_Marker : Element_Type) return Sequence;

   function Has_End_Of_Sequence_Marker (Item : Sequence) return Boolean;

   function Dump (Seq  : Sequence;
                  From : Cursor := Beginning) return Element_Array;

   procedure Clear (Seq : in out Sequence)
     with
       Post =>
         Seq.Length = 0
         and not Seq.Saved_Position
         and Seq.Current_Position = Beginning;

   function Index (Seq : Sequence) return Positive;

   procedure Append (Seq      : in out Sequence;
                     Elements : Element_Array)
     with
       Post => Seq.Length = Seq.Length'Old + Elements'Length;

   procedure Append (To   : in out Sequence;
                     What : Element_Type)
     with
       Post => To.Length = To.Length'Old + 1;

   procedure Append (To   : in out Sequence;
                     What : Sequence)
     with
       Post => To.Length = To.Length'Old + What.Length;

   procedure Rewind (Seq : in out Sequence;
                     To  :        Cursor := Beginning);

   function Length (Seq : Sequence) return Natural;

   function Remaining (Seq : Sequence) return Natural
     with Post => Remaining'Result <= Seq.Length;
   -- Return the number of elements that still need to be read from
   -- Seq.  This includes also the current element

   function Current_Position (Seq : Sequence) return Cursor;

   procedure Set_Position (Seq : in out Sequence;
                           Pos : Cursor);

   function Saved_Position (Seq : Sequence)return Boolean;

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

   function Next (Seq : in out Sequence) return Element_Type;


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

   Beginning : constant Cursor := Cursor'First;

   package Buffer_Holders is
     new Ada.Containers.Indefinite_Holders (Buffer_Type);

   type Sequence is tagged
      record
         Buffer         : Buffer_Holders.Holder;
         First          : Cursor;
         After_Last     : Cursor;
         Position       : Cursor;
         Old_Position   : Cursor;
         Position_Saved : Boolean := False;
         Has_End_Marker : Boolean := False;
         End_Marker     : Element_Type;
      end record
     with
       Type_Invariant =>
         not Sequence.Buffer.Is_Empty
         and then Sequence.First = Sequence.Buffer.Element'First
         and then Sequence.After_Last <= Sequence.Buffer.Element'Last + 1
         and then Sequence.Position >= Sequence.First
         and then Sequence.Position <= Sequence.After_Last;

   function Saved_Position (Seq : Sequence)return Boolean
   is (Seq.Position_Saved);


   function Current_Position (Seq : Sequence) return Cursor
   is (Seq.Position);

   function Length (Seq : Sequence) return Natural
   is (Natural (Seq.Buffer.Element'Length));

   function Read (Seq   : Sequence;
                  Ahead : Natural := 0) return Element_Type
   is (if Seq.Remaining > Ahead then
          Seq.Buffer.Element (Cursor (Integer (Seq.Position) + Ahead))
       elsif Seq.Has_End_Marker then
          Seq.End_Marker
       else
          raise Beyond_End);

   function Has_End_Of_Sequence_Marker (Item : Sequence) return Boolean
   is (Item.Has_End_Marker);

   Empty_Buffer : constant Buffer_Type (1 .. 1) := (others => <>);

   function Empty_Sequence return Sequence
   is (Sequence'(Buffer         => Buffer_Holders.To_Holder (Empty_Buffer),
                 Position       => Empty_Buffer'First,
                 First          => Empty_Buffer'First,
                 After_Last     => Empty_Buffer'Last + 1,
                 Old_Position   => <>,
                 Position_Saved => False,
                 Has_End_Marker => False,
                 End_Marker     => <>));

   function Index (Seq : Sequence) return Positive
   is (Positive (Seq.Position));
end Readable_Sequences.Generic_Sequences;
