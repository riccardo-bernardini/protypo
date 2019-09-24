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

   procedure Unexpected_Token (Found    : Token_Class;
                               Expected : Token_Class)
   is
   begin
      Put_Line (Standard_Error, "Unexpected " & Found'Image & "instead of " & Expected'Image);
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

   ------------
   -- Expect --
   ------------

   procedure Expect_And_Eat (Input    : in out Scanning.Token_List;
                             Expected : Token_Class)
   is
   begin
      if Class (Input.Read) /= Expected then
         Unexpected_Token (Class (Input.Read), Expected);
      end if;

      Input.Next;
   end Expect_And_Eat;

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
         Expect_And_Eat (Input, Open_Parenthesis);

         Indexes := Parse_Expression_List (Input, (others => True));
         Result := Code_Trees.Function_Call (Result, Indexes.Dump);

         Expect_And_Eat (Input, Close_Parenthesis);
      end Parse_Indexed_Name;

      procedure Parse_Selector is
      begin
         Expect_And_Eat (Input, Dot);

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

            Expect_And_Eat (Input, Close_Parenthesis);

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

   function Parse_Assign (Input : in out Scanning.Token_List;
                          Names : Statement_Sequences.Sequence)
                          return Code_Trees.Parsed_Code
   is
      Values : Statement_Sequences.Sequence;
   begin
      Expect_And_Eat (Input, Assign);

      Values := Parse_Expression_List (Input, (others => True));

      if Names.Length /= Values.Length then
         raise Constraint_Error;
      end if;

      Expect_And_Eat (Input, End_Of_Statement);

      return Code_Trees.Assignment (LHS        => Names.Dump,
                                    Expression => Values.Dump);
   end Parse_Assign;

   -----------------------
   -- Parse_Conditional --
   -----------------------

   function Parse_Conditional (Input : in out Scanning.Token_List)
                               return Code_Trees.Parsed_Code
   is
      Branches    : Statement_Sequences.Sequence;
      Conditions  : Statement_Sequences.Sequence;
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

      Expect_And_Eat (Input, Kw_End);
      Expect_And_Eat (Input, Kw_If);
      Expect_And_Eat (Input, End_Of_Statement);

      return Code_Trees.Conditional (Conditions  => Conditions.Dump,
                                     Branches    => Branches.Dump,
                                     Else_Branch => Else_Branch);
   end Parse_Conditional;


   ----------------
   -- Parse_Exit --
   ----------------

   function Parse_Exit (Input : in out Scanning.Token_List)
                        return Code_Trees.Parsed_Code
   is
   begin
      Expect_And_Eat (Input, Kw_Exit);

      declare
         Label : constant String := (if Class (Input.Read) = Identifier then
                                        Value (Input.Next)
                                     else
                                        "");
      begin
         Expect_And_Eat (Input, End_Of_Statement);

         return Code_Trees.Loop_Exit (Label);
      end;
   end Parse_Exit;

   ----------------
   -- Parse_Loop --
   ----------------

   function Parse_Loop (Input : in out Scanning.Token_List;
                        Label : String := "")
                        return Code_Trees.Parsed_Code
   is
      Loop_Body : Code_Trees.Parsed_Code;
   begin
      Expect_And_Eat (Input, Kw_Loop);

      Loop_Body := Parse_Statement_Sequence (Input);

      Expect_And_Eat (Input, Kw_End);
      Expect_And_Eat (Input, Kw_Loop);
      Expect_And_Eat (Input, End_Of_Statement);

      return Code_Trees.Basic_Loop (Loop_Body, Label);
   end Parse_Loop;

   --------------------
   -- Parse_For_Loop --
   --------------------

   function Parse_For_Loop (Input : in out Scanning.Token_List;
                            Label : String := "")
                            return Code_Trees.Parsed_Code
   is
      Variable  : Code_Trees.Parsed_Code;
      Iterator  : Code_Trees.Parsed_Code;
      Loop_Body : Code_Trees.Parsed_Code;
   begin
      Expect_And_Eat (Input, Kw_For);

      if Class (Input.Read) /= Identifier then
         Unexpected_Token (Class (Input.Read), Identifier);
         raise Constraint_Error;
      end if;

      Variable := Code_Trees.Identifier (Value (Input.Next));

      Expect_And_Eat (Input, Kw_In);

      Iterator := Parse_Expression (Input, (others => True));

      Loop_Body := Parse_Loop (Input, Label);

      return Code_Trees.For_Loop (Variable  => Variable,
                                  Iterator  => Iterator,
                                  Loop_Body => Loop_Body);
   end Parse_For_Loop;


   ----------------------
   -- Parse_While_Loop --
   ----------------------

   function Parse_While_Loop (Input : in out Scanning.Token_List;
                              Label : String := "")
                              return Code_Trees.Parsed_Code
   is
      Condition : Code_Trees.Parsed_Code;
      Loop_Body : Code_Trees.Parsed_Code;
   begin
      Expect_And_Eat (Input, Kw_While);

      Condition := Parse_Expression (Input, (others => True));

      Loop_Body := Parse_Loop (Input, Label);

      return Code_Trees.While_Loop (Condition  => Condition,
                                    Loop_Body  => Loop_Body);
   end Parse_While_Loop;

   ------------------
   -- Parse_Return --
   ------------------

   function Parse_Return (Input : in out Scanning.Token_List)
                          return Code_Trees.Parsed_Code
   is
      Return_List : Statement_Sequences.Sequence;
   begin
      Expect_And_Eat (Input, Kw_Return);

      if Class (Input.Read) = End_Of_Statement then
         Return_List := Statement_Sequences.Empty_Sequence;
      else
         Return_List := Parse_Expression_List (Input, (others => True));
      end if;

      Expect_And_Eat (Input, End_Of_Statement);

      return Code_Trees.Return_To_Caller (Return_List.Dump);

   end Parse_Return;

   ------------------------------
   -- Parse_Statement_Sequence --
   ------------------------------

   function Parse_Statement_Sequence
     (Input      : in out Scanning.Token_List;
      Valid_End  : Token_Mask) return Code_Trees.Parsed_Code
   is
      subtype Labeled_Construct is Unvalued_Token
        with
          Static_Predicate => Labeled_Construct in Kw_Loop | Kw_While | Kw_For;

      function Parse_Labeled_Construct (Input : in out Scanning.Token_List)
                                        return Code_Trees.Parsed_Code
      is
         Label : constant String := Value (Input.Next);
      begin
         Expect_And_Eat (Input, Label_Separator);

         case Labeled_Construct (Class (Input.Read)) is
            when Kw_Loop =>
               return Parse_Loop (Input, Label);

            when Kw_For =>
               return Parse_For_Loop (Input, Label);

            when Kw_While =>
               return Parse_While_Loop (Input, Label);
         end case;
      end Parse_Labeled_Construct;

      function Parse_Assign_Or_Call (Input : in out Scanning.Token_List)
                                     return Code_Trees.Parsed_Code
      is
         subtype Name_List_Follower is Unvalued_Token
           with
             Static_Predicate =>
               Name_List_Follower in End_Of_Statement | Assign;

         Names : constant Statement_Sequences.Sequence :=
                   Parse_Name_List (Input);
      begin
         if not (Class (Input.Read) in Name_List_Follower) then
            raise Constraint_Error;
         end if;

         case Name_List_Follower (Class (Input.Read)) is
            when End_Of_Statement =>

               Expect_And_Eat (Input, End_Of_Statement);

               if Names.Length /= 1 then
                  raise Constraint_Error;
               else
                  return Code_Trees.Procedure_Call (Names.Read);
               end if;

            when Assign =>
               return Parse_Assign (Input, Names);
         end case;
      end Parse_Assign_Or_Call;

      Result : Statement_Sequences.Sequence;


   begin
      loop
         exit when Valid_End (Class (Input.Read));

         case Class (Input.Read) is
            when Open_Naked =>
               Result.Append (Parse_Naked (Input));

            when Identifier =>
               if Class (Input.Read (1)) = Label_Separator then
                  if not (Class (Input.Read (2)) in Labeled_Construct) then
                     raise Constraint_Error;
                  else
                     Result.Append (Parse_Labeled_Construct (Input));
                  end if;
               else
                  Result.Append (Parse_Assign_Or_Call (Input));
               end if;

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

            when Kw_Exit =>
               Result.Append (Parse_Exit (Input));

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
                 Real             | Kw_Xor           | Kw_Not              |
                 Label_Separator
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
