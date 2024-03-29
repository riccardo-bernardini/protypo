with Ada.Containers.Vectors;
with Readable_Sequences.Generic_Sequences;

package Readable_Sequences.String_Sequences is
   package Basic_String_Sequences is
     new Generic_Sequences (Element_Type  => Character,
                            Element_Array => String);

   type Sequence (<>) is
     new Basic_String_Sequences.Sequence
   with
     private;

   subtype Cursor is Basic_String_Sequences.Cursor;

   Beyond_End : exception renames Basic_String_Sequences.Beyond_End;

   type Position_Type is private;

   No_Position : constant Position_Type;

   function Image (Pos : Position_Type;
                   Full : Boolean := True) return String;

   function Line (Pos : Position_Type) return Positive
     with Pre => Pos /= No_Position;

   function Char (Pos : Position_Type) return Positive
     with Pre => Pos /= No_Position;

   overriding
   function Empty_Sequence return sequence;

   overriding
   function Create (Init : String) return Sequence;

   overriding
   function Create (End_Of_Sequence_Marker : Character) return Sequence;

   overriding
   function Create (Init                   : String;
                    End_Of_Sequence_Marker : Character) return Sequence;


   function Position (Item : in out Sequence) return Position_Type;
private
   type Position_Type is
      record
         Line : Natural;
         Char : Natural;
      end record;


   No_Position : constant Position_Type := (Line => 0, Char => 0);

   function Line (Pos : Position_Type) return Positive
   is (Pos.Line);

   function Char (Pos : Position_Type) return Positive
   is (Pos.Char);

   function Image (Pos : Position_Type;
                   Full : Boolean := True) return String
   is (if Pos = No_Position then
          "<unknown>"
       else
          "Line "
       & Pos.Line'Image
       & (if Full then
             ", column: " & Pos.Char'Image
          else
             ""));

   package Position_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Position_Type);

   type Sequence is
     new Basic_String_Sequences.Sequence
   with
      record
         Position_Cache : Position_Vectors.Vector;
      end record;
end Readable_Sequences.String_Sequences;
