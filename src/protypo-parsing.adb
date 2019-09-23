pragma Ada_2012;
with Ada.Text_IO;                           use Ada.Text_IO;
with Ada.Strings.Unbounded;                 use Ada.Strings.Unbounded;

with Protypo.Tokens;                        use Protypo.Tokens;

with Readable_Sequences.Generic_Sequences;  use Readable_Sequences;


package body Protypo.Parsing is

   package Statement_Sequences is
        new Generic_Sequences (Element_Type  => Code_Trees.Parsed_Code,
                               Element_Array => Code_Trees.Tree_Array);

   type Token_Mask is array (Token_Class) of Boolean;

   -----------
   -- Image --
   -----------

   function Image (X : Token_Mask) return String is
      Result : Unbounded_String;
   begin
      for Tk in X'Range loop
         if X (Tk) then
            if Result = Null_Unbounded_String then
               Result := To_Unbounded_String (Tk'Image);
            else
               Result := Result & " or " & Tk'Image;
            end if;
         end if;
      end loop;

      if Result = Null_Unbounded_String then
         return "(nothing)";
      else
         return To_String (Result);
      end if;
   end Image;

   procedure Unexpected_Token (Found    : Token_Class;
                               Expected : Token_Mask)
   is
   begin
      Put_Line (Standard_Error, "Unexpected " & Found'Image & "instead of " & Image (Expected));
   end Unexpected_Token;


   procedure Expect (Input    : in out Scanning.Token_List;
                     Expected : Token_Mask)
   is
   begin
      if not Expected (Class (Input.Read)) then
         Unexpected_Token (Class (Input.Read), Expected);
      end if;

      Input.Next;
   end Expect;

   procedure Expect (Input    : in out Scanning.Token_List;
                     Expected : Token_Class)
   is
      Mask : Token_Mask := (others => False);
   begin
      Mask(Expected) := True;
      Expect (Input, Mask);
   end Expect;

    function Parse_Expression
     (Input      : in out Scanning.Token_List;
      Valid_End  : Token_Mask)
      return Code_Trees.Parsed_Code
   is
      subtype Logical_Operator is Token_Class range Kw_And .. Kw_Or;

      Result : Code_Trees.Parsed_Code;
      Mask   : Token_Mask := Valid_End;
      Op : Logical_Operator;
   begin
      Mask (Kw_And) := True;
      Mask (Kw_Or) := True;

      Result := Parse_Relation (Input, Mask);

      if Class (Input.Read) in Logical_Operator then
         Op := Class (Input.Read);
         Mask := Valid_End;
         Mask(Op) := True;

         while Class (Input.Read) = Op loop
            Input.Next;
            Result := Code_Trees.Binary_Operator (Result,
                                                  Parse_Relation (Input, Mask),
                                                  Op);
         end loop;
      end if;

      if not Valid_End (Class (Input.Read)) then
         Unexpected_Token (Class (Input.Read), Valid_End);
         Input.Next;
      end if;

      return Result;
   end Parse_Expression;

   ---------------------------
   -- Parse_Expression_List --
   ---------------------------

   function Parse_Expression_List
     (Input      : in out Scanning.Token_List;
      Valid_End  : Token_Mask) return Statement_Sequences.Sequence
   is
      Result : Statement_Sequences.Sequence;
      Mask : Token_Mask := Valid_End;
   begin
      Mask (Comma) := True;
      Result.Append (Parse_Expression (Input, Mask));

      while Class (Input.Read) = Comma loop
         Input.Next;
         Result.Append (Parse_Expression (Input, Mask));
      end loop;

      Expect (Input, Valid_End);
      Input.Next;

      return Result;
   end Parse_Expression_List;

   -----------------
   -- Parse_Naked --
   -----------------

   function Parse_Naked (Input : in out Scanning.Token_List)
                         return Code_Trees.Parsed_Code
   is
      Result : Statement_Sequences.Sequence;
   begin
      Expect (Input, Open_Naked);

      Result := Parse_Expression_List (Input, (Close_Naked => True, others => False));

      Expect (Input, Close_Naked);

      return Code_Trees.Naked_Expression (Result.Dump);
   end Parse_Naked;

   ------------------------------
   -- Parse_Statement_Sequence --
   ------------------------------

   function Parse_Statement_Sequence
     (Input      : in out Scanning.Token_List;
      Valid_End  : Token_Mask) return Code_Trees.Parsed_Code
   is

      Result : Statement_Sequences.Sequence;
   begin
      loop
         exit when Valid_End (Class (Input.Read));

         case Class (Input.Read)	 is
            when Open_Naked =>
               Result.Append (Parse_Naked (Input));

            when Identifier =>
               Result.Append (Parse_Assign (Input));

            when Kw_If =>
               Result.Append (Parse_Conditional (Input));

            when Kw_Case =>
               Result.Append (Parse_Case (Input));

            when Kw_For =>
               Result.Append (Parse_For_Loop (Input));

            when Kw_While =>
               Result.Append (Parse_While_Loop (Input));

            when Kw_Loop =>
               Result.Append (Parse_Loop (Input));


            when Kw_Return =>
               Result.Append (Parse_Return (Input));


            when Int              | Text             | Plus  | Minus       |
                 Mult             | Div              | Equal |  Different  |
                 Less_Than        | Greater_Than     | Less_Or_Equal       |
                 Greater_Or_Equal | Assign           | Dot                 |
                 Comma            | End_Of_Statement | Close_Parenthesis   |
                 Kw_Then          | Kw_Elsif         | Kw_Else             |
                 Kw_End           | Kw_And           | Kw_Or               |
                 Open_Parenthesis | Close_Naked      | End_Of_Text         |
                 Kw_When          | Kw_In            | Kw_Of               |
                 Real =>

               Unexpected_Token (Class (Input.Read), Valid_End);
               exit;
         end case;
      end loop;

      return Code_Trees.Statement_Sequence (Statement_Sequences.Dump (Result));
   end Parse_Statement_Sequence;

   ------------------------------
   -- Parse_Statement_Sequence --
   ------------------------------

   function Parse_Statement_Sequence
     (Input      : in out Scanning.Token_List)
      return Code_Trees.Parsed_Code
   is
   begin
      return Parse_Statement_Sequence (Input, (End_Of_Text => True, others => False));
   end Parse_Statement_Sequence;

end Protypo.Parsing;
