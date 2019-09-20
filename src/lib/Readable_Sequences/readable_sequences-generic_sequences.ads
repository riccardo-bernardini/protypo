with Ada.Containers.Vectors;

generic
   type Element_Type is private;
package Readable_Sequences.Generic_Sequences is
   type Sequence is tagged private;
   type Cursor is private;

   Beginning : constant Cursor;

   procedure Append (To   : in out Sequence;
                     What : Element_Type);

   procedure Rewind (Seq : in out Sequence;
                     To  :        Cursor := Beginning);

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

   function End_Of_Sequence (Seq : Sequence) return Boolean;

   function Read (Seq : Sequence) return Element_Type
     with
       Pre => not Seq.End_Of_Sequence;

   procedure Next (Seq : in out Sequence);
private

   type Cursor is range 1 .. Integer'Last;

   Beginning : constant Cursor := Cursor'First;

   package Element_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Cursor,
                                 Element_Type => Element_Type);

   type Sequence is tagged
      record
         Vector         : Element_Vectors.Vector;
         Position       : Cursor;
         Old_Position   : Cursor;
         Position_Saved : Boolean;
      end record;

   function Saved_Position (Seq : Sequence)return Boolean
   is (Seq.Position_Saved);


   function current_Position (Seq : Sequence) return Cursor
   is (Seq.Position);

   function End_Of_Sequence (Seq : Sequence) return Boolean
   is (Seq.Position > Seq.Vector.Last_Index);

   function Read (Seq : Sequence) return Element_Type
   is (if not Seq.End_Of_Sequence then
          Seq.Vector (Seq.Position)
       else
          raise Constraint_Error);


end Readable_Sequences.Generic_Sequences;
