with Ada.Containers.Vectors;

generic
   type Element_Type is private;
   type Element_Array is array (Positive range <>) of Element_Type;
package Readable_Sequences.Generic_Sequences is
   type Sequence is tagged private;
   type Cursor is private;

   Beginning : constant Cursor;

   function Create (Init : Element_Array) return Sequence;

   function Dump (Seq : Sequence) return Element_Array;

   procedure Clear (Seq : in out Sequence)
     with
       Post =>
         Seq.Length = 0
         and not Seq.Saved_Position
         and Seq.Current_Position = Beginning;

   procedure Append (Seq      : in out Sequence;
                     Elements : Element_Array)
     with
       Post => seq.Length = seq.Length'Old + Elements'Length;

   procedure Append (To   : in out Sequence;
                     What : Element_Type)
     with
       Post => To.Length = To.Length'Old + 1;

   procedure Rewind (Seq : in out Sequence;
                     To  :        Cursor := Beginning);

   function Length (Seq : Sequence) return Natural;

   function Remaining (Seq : Sequence) return Natural
     with Post => Remaining'Result <= Seq.Length;
   -- Return the number of elements that still need to be read from
   -- Seq.  This includes also the current element

   function Current_Position (Seq : Sequence) return Cursor;

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
       Pre => Seq.Remaining > Ahead;


   procedure Next (Seq  : in out Sequence;
                   Step : Positive := 1)
     with
       Post => Seq.Remaining = Integer'Max (Seq.Remaining'Old - Step, 0);

   procedure Back (Seq : in out Sequence;
                   Step : Positive := 1)
     with
       Post => Seq.Remaining = Integer'Min (Seq.Remaining'Old + Step, Seq.Length);

   procedure Process (Seq : Sequence;
                      Callback : access procedure (Item : Element_Type));
private

   type Cursor is range 1 .. Integer'Last;

   Beginning : constant Cursor := Cursor'First;

   package Element_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Cursor,
                                 Element_Type => Element_Type);

   type Sequence is tagged
      record
         Vector         : Element_Vectors.Vector := Element_Vectors.Empty_Vector;
         Position       : Cursor := Beginning;
         Old_Position   : Cursor;
         Position_Saved : Boolean := False;
      end record;

   function Saved_Position (Seq : Sequence)return Boolean
   is (Seq.Position_Saved);


   function Current_Position (Seq : Sequence) return Cursor
   is (Seq.Position);

   function Length (Seq : Sequence) return Natural
   is (Natural (Seq.Vector.Length));

   function Remaining (Seq : Sequence) return Natural
   is (Integer (Seq.Vector.Last_Index) - Integer (Seq.Position) + 1);

   function Read (Seq : Sequence;
                  Ahead : Natural := 0) return Element_Type
   is (if Seq.Remaining > Ahead then
          Seq.Vector (Cursor (Positive (Seq.Position) + Ahead))
       else
          raise Constraint_Error);


end Readable_Sequences.Generic_Sequences;
