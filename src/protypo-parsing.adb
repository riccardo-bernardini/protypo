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



   ------------
   -- Expect --
   ------------

   procedure Expect (Input    : in out Scanning.Token_List;
                     Expected : Token_Mask)
   is
   begin
      if not Expected (Class (Input.Read)) then
         Unexpected_Token (Class (Input.Read), Expected);
      end if;
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect (Input    : in out Scanning.Token_List;
                     Expected : Token_Class)
   is
      Mask : Token_Mask := (others => False);
   begin
      Mask (Expected) := True;
      Expect (Input, Mask);
   end Expect;

   function Parse_Expression
     (Input      : in out Scanning.Token_List;
      Valid_End  : Token_Mask)
      return Code_Trees.Parsed_Code;

   function Parse_Expression_List
     (Input      : in out Scanning.Token_List;
      Valid_End  : Token_Mask) return Statement_Sequences.Sequence;


   -------------------
   -- Parse_Primary --
   -------------------

   function Parse_Name
     (Input      : in out Scanning.Token_List)
      return Code_Trees.Parsed_Code
   is
      Result  : Code_Trees.Parsed_Code := Code_Trees.Empty_Tree;
      Indexes : Statement_Sequences.Sequence;

      procedure Parse_Indexed_Name is
      begin
         Expect (Input, Open_Parenthesis);
         Input.Next;

         Indexes := Parse_Expression_List (Input, (others => True));
         Result := Code_Trees.Function_Call (Result, Indexes.Dump);

         Expect (Input, Close_Parenthesis);
         Input.Next;
      end Parse_Indexed_Name;

      procedure Parse_Selector is
      begin
         Expect (Input, Dot);
         Input.Next;

         if Class (Input.Read) /= Identifier then
            raise Constraint_Error;
         end if;

         Result := Code_Trees.Selector (Result, Value (Input.Next));
      end Parse_Selector;
   begin
      pragma Assert (Class (Input.Read) = Identifier);

      Result := Code_Trees.Identifier (Value (Input.Next));

      loop
         if Class (Input.Read) = Open_Parenthesis then
            Parse_Indexed_Name;
         end if;

         if Class (Input.Read) /= Dot then
            exit;
         else
            Parse_Selector;
         end if;
      end loop;

      return Result;
   end Parse_Name;
   -------------------
   -- Parse_Primary --
   -------------------

   function Parse_Primary
     (Input      : in out Scanning.Token_List)
      return Code_Trees.Parsed_Code
   is
      subtype Primary_Head is Token_Class
        with Static_Predicate =>
          Primary_Head in Open_Parenthesis | Identifier | Text | Int | Real;

      Result : Code_Trees.Parsed_Code;
      Mask   : Token_Mask := (others => False);
      --          := (Open_Parenthesis => True,
      --                                         Identifier       => True,
      --                                         Text             => True,
      --                                         Int              => True,
      --                                         Real             => True,
      --
      --        others => False);
   begin
      for K in Primary_Head loop
         Mask (K) := True;
      end loop;

      if not (Class (Input.Read) in Primary_Head) then
         Unexpected_Token (Class (Input.Read), Mask);
         raise Constraint_Error;
      end if;

      case Primary_Head (Class (Input.Read)) is
         when Open_Parenthesis =>
            Input.Next;
            Result := Parse_Expression (Input, (others => True));
            Expect (Input, Close_Parenthesis);
            Input.Next;

         when Identifier =>
            Result := Parse_Name (Input);

         when Text =>
            Result := Code_Trees.String_Constant (Value (Input.Read));

         when Int =>
            Result := Code_Trees.Integer_Constant (Value (Input.Read));

         when Real =>
            Result := Code_Trees.Float_Constant (Value (Input.Read));

      end case;

      return Result;
   end Parse_Primary;
   ------------------
   -- Parse_factor --
   ------------------

   function Parse_Factor
     (Input      : in out Scanning.Token_List)
      return Code_Trees.Parsed_Code
   is
      Result    : Code_Trees.Parsed_Code;
      Op        : Unary_Operator;
      Has_Unary : Boolean;
   begin
      if Class (Input.Read) in Unary_Operator then
         Op := Class (Input.Next);
         Has_Unary := True;
      end if;

      Result := Parse_Primary (Input);

      if not Has_Unary then
         return Result;
      else
         return Code_Trees.Unary_Operation (Result, Op);
      end if;
   end Parse_Factor;
   ----------------
   -- Parse_Term --
   ----------------

   function Parse_Term
     (Input      : in out Scanning.Token_List;
      Valid_End  : Token_Mask)
      return Code_Trees.Parsed_Code
   is
      Result : Code_Trees.Parsed_Code;
      Op     : Binary_Operator;
   begin

      Result := Parse_Factor (Input);

      while Class (Input.Read) in Mult | Div loop
         Op := Class (Input.Next);

         Result := Code_Trees.Binary_Operation
           (Left      => Result,
            Right     => Parse_Factor (Input),
            Operation => Op);
      end loop;

      Expect (Input, Valid_End);
      return Result;
   end Parse_Term;


   -----------------------
   -- Parse_Simple_Expr --
   -----------------------

   function Parse_Simple_Expr
     (Input      : in out Scanning.Token_List;
      Valid_End  : Token_Mask)
      return Code_Trees.Parsed_Code
   is
      Result : Code_Trees.Parsed_Code;
      Mask   : Token_Mask := Valid_End;
      Op     : Binary_Operator;
   begin
      Mask (Plus) := True;
      Mask (Minus) := True;

      Result := Parse_Term (Input, Mask);

      while Class (Input.Read) in Plus | Minus loop
         Op := Class (Input.Next);

         Result := Code_Trees.Binary_Operation (Left      => Result,
                                                Right     => Parse_Term (Input, Valid_End),
                                                Operation => Op);
      end loop;

      return Result;
   end Parse_Simple_Expr;

   --------------------
   -- Parse_relation --
   --------------------

   function Parse_Relation
     (Input      : in out Scanning.Token_List;
      Valid_End  : Token_Mask)
      return Code_Trees.Parsed_Code
   is
      Result : Code_Trees.Parsed_Code;
      Mask   : Token_Mask := Valid_End;
      Op     : Comp_Operator;
   begin
      for K in Comp_Operator loop
         Mask (K) := True;
      end loop;

      Result := Parse_Simple_Expr (Input, Mask);

      if Class (Input.Read) in Comp_Operator then
         Op := Class (Input.Next);

         Result := Code_Trees.Binary_Operation
           (Left      => Result,
            Right     => Parse_Simple_Expr (Input, Valid_End),
            Operation => Op);
      end if;

      Expect (Input, Valid_End);

      return Result;
   end Parse_Relation;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     (Input      : in out Scanning.Token_List;
      Valid_End  : Token_Mask)
      return Code_Trees.Parsed_Code
   is
      Result : Code_Trees.Parsed_Code;
      Mask   : Token_Mask := Valid_End;
      Op     : Logical_Operator;
   begin
      Mask (Kw_And) := True;
      Mask (Kw_Or) := True;

      Result := Parse_Relation (Input, Mask);

      if Class (Input.Read) in Logical_Operator then
         Op := Class (Input.Read);
         Mask := Valid_End;
         Mask (Op) := True;

         while Class (Input.Read) = Op loop
            Input.Next;
            Result := Code_Trees.Binary_Operation (Left      => Result,
                                                   Right     => Parse_Relation (Input, Mask),
                                                   Operation => Op);
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
      Mask   : Token_Mask := Valid_End;
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

   ---------------------
   -- Parse_name_List --
   ---------------------

   function Parse_Name_List
     (Input      : in out Scanning.Token_List) return Statement_Sequences.Sequence
   is
      Result : Statement_Sequences.Sequence;
   begin
      Result.Append (Parse_Name (Input));

      while Class (Input.Read) = Comma loop
         Input.Next;
         Result.Append (Parse_Name (Input));
      end loop;

      return Result;
   end Parse_Name_List;

   ------------------
   -- Parse_Assign --
   ------------------

   function Parse_Assign (Input : in out Scanning.Token_List)
                          return Code_Trees.Parsed_Code
   is
      Names  : Statement_Sequences.Sequence;
      Values : Statement_Sequences.Sequence;
   begin
      Names := Parse_Name_List (Input);

      Expect (Input, Assign);
      Input.Next;

      Values := Parse_Expression_List (Input, (others => True));

      if Names.Length /= Values.Length then
         raise Constraint_Error;
      end if;

      return Code_Trees.Assignment (LHS        => Names.Dump,
                                    Expression => Values.Dump);
   end Parse_Assign;

   -----------------------
   -- Parse_Conditional --
   -----------------------

   function Parse_Conditional (Input : in out Scanning.Token_List)
                               return Code_Trees.Parsed_Code
   is
      Branches : Statement_Sequences.Sequence;
      Conditions : Statement_Sequences.Sequence;
      Else_Branch : Code_Trees.Parsed_Code := Code_Trees.Empty_Tree;
   begin
      Expect (Input, Kw_If);
      Input.Next;

      Conditions.Append (Parse_Expression (Input, (others => True)));

      Expect (Input, Kw_Then);
      Input.Next;

      Branches.Append (Parse_Statement_Sequence (Input));

      while Class (Input.Read) = Kw_Elsif loop
         Input.Next;

         Conditions.Append (Parse_Expression (Input, (others => True)));
         Branches.Append (Parse_Statement_Sequence (Input));
      end loop;

      if Class (Input.Read) = Kw_Else then
         Input.Next;
         Else_Branch := Parse_Statement_Sequence (Input);
      end if;

      Expect (Input, Kw_End);
      Input.Next;

      Expect (Input, Kw_If);
      Input.Next;

      return Code_Trees.Conditional (Conditions  => Conditions.Dump,
                                     Branches    => Branches.Dump,
                                     Else_Branch => Else_Branch);
   end Parse_Conditional;


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

         case Class (Input.Read) is
            when Open_Naked =>
               Result.Append (Parse_Naked (Input));

            when Identifier =>
               Result.Append (Parse_Assign (Input));

            when Kw_If =>
               Result.Append (Parse_Conditional (Input));

            when Kw_Case =>
               raise Program_Error with "Unimplemented";

               --                 Result.Append (Parse_Case (Input));

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
                 Real             | Kw_Xor           | Kw_Not
               =>

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
