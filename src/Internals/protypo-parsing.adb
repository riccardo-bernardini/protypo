pragma Ada_2012;
pragma Warnings (Off, "no entities");
with Ada.Text_IO;                           use Ada.Text_IO;
with Ada.Exceptions;

with Protypo.Tokens;                        use Protypo.Tokens;

with Readable_Sequences.Generic_Sequences;  use Readable_Sequences;


package body Protypo.Parsing is
   use Ada.Strings.Unbounded;

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
                               Expected : Token_Mask;
                               Position : Token_Position)
   is
   begin
      raise Parsing_Error
        with "Unexpected "
        & Found'Image & " instead of " & Image (Expected)
        & " at " & Image (Position);
   end Unexpected_Token;

   procedure Unexpected_Token (Found    : Token_Class;
                               Expected : Token_Class;
                               Position : Token_Position)
   is
   begin
      raise Parsing_Error
        with "Unexpected "
        & Found'Image & " instead of " & Expected'Image
        & " at " & Image (Position);
   end Unexpected_Token;



   ------------
   -- Expect --
   ------------

   procedure Expect (Input    : in Scanning.Token_List;
                     Expected : Token_Mask)
   is
   begin
      if not Expected (Class (Input.Read)) then
         Unexpected_Token (Class (Input.Read), Expected, Position (Input.Read));
      end if;
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect (Input    : in Scanning.Token_List;
                     Expected : Token_Class)
   is
      Mask : Token_Mask := (others => False);
   begin
      Mask (Expected) := True;
      Expect (Input, Mask);
   end Expect;
   pragma Unreferenced (Expect);

   ------------
   -- Expect --
   ------------

   procedure Expect_And_Eat (Input    : in out Scanning.Token_List;
                             Expected : Token_Class;
                             Context  : String := "")
   is
   begin
      if Class (Input.Read) /= Expected then
         raise Parsing_Error
           with "Expecting  "
           & Expected'Image
           & " found "
           &  Class (Input.Read)'Image
           & " at " & Image (Position (Input.Read))
           & (if Context /= "" then " (" & Context & ")" else "");
      end if;

      Input.Next;
   end Expect_And_Eat;



   function Parse_Expression_List
     (Input      : in out Scanning.Token_List)
      return Statement_Sequences.Sequence;


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

         Indexes := Parse_Expression_List (Input);
         Result := Code_Trees.Indexed_Name (Result, Indexes.Dump);

         Expect_And_Eat (Input, Close_Parenthesis);
      end Parse_Indexed_Name;

      procedure Parse_Selector is
      begin
         Expect_And_Eat (Input, Dot);

         if Class (Input.Read) /= Identifier then
            raise Parsing_Error
              with "Unexpected token "
              &  Class (Input.Read)'Image
              & " in FIELD part"
              & " at " & Image (Position (Input.Read)) ;
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
          Primary_Head in Open_Parenthesis | Identifier | Text | Int | Real | Kw_Capture;

      Result : Code_Trees.Parsed_Code;
   begin
      --  Put_Line (">> parse primary");

      if not (Class (Input.Read) in Primary_Head) then
         raise Parsing_Error
           with "Unexpected token "
           &  Class (Input.Read)'Image
           & " in primary expression"
           & " at " & Image (Position (Input.Read));
      end if;

      --  Put_Line (Image (Input.Read));

      case Primary_Head (Class (Input.Read)) is
         when Open_Parenthesis =>
            Input.Next;
            Result := Parse_Expression (Input);

            Expect_And_Eat (Input, Close_Parenthesis, "PRIMARY");

         when Identifier =>
            Result := Parse_Name (Input);

         when Text =>
            Result := Code_Trees.String_Constant (Value (Input.Next));

         when Int =>
            Result := Code_Trees.Integer_Constant (Value (Input.Next));

         when Real =>
            Result := Code_Trees.Float_Constant (Value (Input.Next));

         when Kw_Capture =>
            Input.Next;
            Expect_And_Eat (Input, Open_Parenthesis, "PRIMARY 2");

            if Class (Input.Read) /= Identifier then
               raise Parsing_Error with "Expected identifier inside CAPTURE call at " & Image (Position (Input.Read));
            end if;

            declare
               Parameters : Statement_Sequences.Sequence;
               Here       : constant Tokens.Token_Position := Tokens.Position (Input.Read);

               Name : constant Unbounded_Id  :=
                        Unbounded_Id'(To_Unbounded_String (Value (Input.Next)));
            begin
               Expect_And_Eat (Input, Open_Parenthesis, "PRIMARY 3");

               Parameters := Parse_Expression_List (Input);

               Result := Code_Trees.Capture (Name, Parameters.Dump, Here);

               -- The close parenthesis of the called function
               Expect_And_Eat (Input, Close_Parenthesis, "PRIMARY 4");

               -- Close of CAPTURE
               Expect_And_Eat (Input, Close_Parenthesis, "PRIMARY 5");
               --  Put_Line (">> done capture");
            end;


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
     (Input      : in out Scanning.Token_List)
      return Code_Trees.Parsed_Code
   is
      Result : Code_Trees.Parsed_Code;
      Op     : Binary_Operator;
   begin

      Result := Parse_Factor (Input);

      while Class (Input.Read) in Mult | Div | Kw_Mod loop
         Op := Class (Input.Next);

         Result := Code_Trees.Binary_Operation
           (Left      => Result,
            Right     => Parse_Factor (Input),
            Operation => Op);
      end loop;

      return Result;
   end Parse_Term;


   -----------------------
   -- Parse_Simple_Expr --
   -----------------------

   function Parse_Simple_Expr
     (Input      : in out Scanning.Token_List)
      return Code_Trees.Parsed_Code
   is
      Result : Code_Trees.Parsed_Code;
      Op     : Binary_Operator;
   begin

      Result := Parse_Term (Input);

      while Class (Input.Read) in Plus | Minus loop
         Op := Class (Input.Next);

         Result := Code_Trees.Binary_Operation (Left      => Result,
                                                Right     => Parse_Term (Input),
                                                Operation => Op);
      end loop;

      return Result;
   end Parse_Simple_Expr;

   --------------------
   -- Parse_relation --
   --------------------

   function Parse_Relation
     (Input      : in out Scanning.Token_List)
      return Code_Trees.Parsed_Code
   is
      Result : Code_Trees.Parsed_Code;
      Op     : Comp_Operator;
   begin
      Result := Parse_Simple_Expr (Input);

      if Class (Input.Read) in Comp_Operator then
         Op := Class (Input.Next);

         Result := Code_Trees.Binary_Operation
           (Left      => Result,
            Right     => Parse_Simple_Expr (Input),
            Operation => Op);
      end if;


      return Result;
   end Parse_Relation;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     (Input      : in out Scanning.Token_List)
      return Code_Trees.Parsed_Code
   is
      Result : Code_Trees.Parsed_Code;
      Op     : Logical_Operator;
      Here   : constant Tokens.Token_Position := Tokens.Position (Input.Read);
   begin
      --  Put_Line (">> Parse expression");

      Result := Parse_Relation (Input);

      if Class (Input.Read) in Logical_Operator then
         Op := Class (Input.Read);

         while Class (Input.Read) = Op loop
            Input.Next;
            Result := Code_Trees.Binary_Operation (Left      => Result,
                                                   Right     => Parse_Relation (Input),
                                                   Operation => Op,
                                                   Position  => Here);
         end loop;
      end if;

      return Result;
   end Parse_Expression;

   ---------------------------
   -- Parse_Expression_List --
   ---------------------------

   function Parse_Expression_List
     (Input      : in out Scanning.Token_List)
      return Statement_Sequences.Sequence
   is
      Result : Statement_Sequences.Sequence;
   begin
      Result.Append (Parse_Expression (Input));

      while Class (Input.Read) = Comma loop
         Input.Next;
         Result.Append (Parse_Expression (Input));
      end loop;


      return Result;
   end Parse_Expression_List;



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
      Values : Statement_Sequences.Sequence;
      Names  : constant Statement_Sequences.Sequence := Parse_Name_List (Input);
   begin
      Expect_And_Eat (Input, Assign, "ASSIGN 1");

      Values := Parse_Expression_List (Input);

      Expect_And_Eat (Input, End_Of_Statement, "ASSIGN 2");


      return Code_Trees.Assignment (LHS    => Names.Dump,
                                    Value  => Values.Dump);
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
      Expect_And_Eat (Input, Kw_If);

      Conditions.Append (Parse_Expression (Input));

      Expect_And_Eat (Input, Kw_Then);

      Branches.Append (Parse_Statement_Sequence (Input));

      while Class (Input.Read) = Kw_Elsif loop
         Input.Next;

         Conditions.Append (Parse_Expression (Input));

         Expect_And_Eat (Input, Kw_Then);

         Branches.Append (Parse_Statement_Sequence (Input));
      end loop;

      if Class (Input.Read) = Kw_Else then
         Input.Next;
         Else_Branch := Parse_Statement_Sequence (Input);
      end if;

      Expect_And_Eat (Input, Kw_End);
      Expect_And_Eat (Input, Kw_If);
      Expect_And_Eat (Input, End_Of_Statement);

      return Code_Trees.If_Then_Else (Conditions    => Conditions.Dump,
                                      Then_Branches => Branches.Dump,
                                      Else_Branch   => Else_Branch);
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

      --  Put_Line ("In parse loop" & Image (Input.Read));

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
      Iterator  : Code_Trees.Parsed_Code;
      Loop_Body : Code_Trees.Parsed_Code;
      Here      : constant Tokens.Token_Position := Tokens.Position (Input.Read);
   begin
      Expect_And_Eat (Input, Kw_For);

      if Class (Input.Read) /= Identifier then
         raise Parsing_Error
           with "Expecting FOR variable, found "
           &  Class (Input.Read)'Image
           & " at " & Image (Here);
      end if;

      declare
         Variable : constant String := Value (Input.Next);
      begin
         Expect_And_Eat (Input, Kw_In);

         Iterator := Parse_Expression (Input);

         Loop_Body := Parse_Loop (Input, Label);

         return Code_Trees.For_Loop (Variable  => Variable,
                                     Iterator  => Iterator,
                                     Loop_Body => Loop_Body,
                                     Position  => Here);
      end;
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
      Here      : constant Tokens.Token_Position := Tokens.Position (Input.Read);
   begin
      Expect_And_Eat (Input, Kw_While);

      Condition := Parse_Expression (Input);

      Loop_Body := Parse_Loop (Input, Label);

      return Code_Trees.While_Loop (Condition  => Condition,
                                    Loop_Body  => Loop_Body,
                                    Position   => Here);
   end Parse_While_Loop;

   ------------------
   -- Parse_Return --
   ------------------

   function Parse_Return (Input : in out Scanning.Token_List)
                          return Code_Trees.Parsed_Code
   is
      Return_List : Statement_Sequences.Sequence;
      Here        : constant Tokens.Token_Position := Tokens.Position (Input.Read);
   begin
      Expect_And_Eat (Input, Kw_Return);

      if Class (Input.Read) = End_Of_Statement then
         Return_List := Statement_Sequences.Empty_Sequence;
      else
         Return_List := Parse_Expression_List (Input);
      end if;

      Expect_And_Eat (Input, End_Of_Statement);

      return Code_Trees.Return_To_Caller (Return_List.Dump, Here);

   end Parse_Return;

   ------------------------------
   -- Parse_Statement_Sequence --
   ------------------------------

   function Parse_Statement_Sequence
     (Input      : in out Scanning.Token_List)
      return Code_Trees.Parsed_Code
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
        with
          Pre => Class (Input.Read) = Identifier;

      function Parse_Assign_Or_Call (Input : in out Scanning.Token_List)
                                     return Code_Trees.Parsed_Code
      is
         subtype ID_Follower is Unvalued_Token
           with
             Static_Predicate =>
               ID_Follower in
                 End_Of_Statement | Assign | Open_Parenthesis | Dot | Comma;

         Follower  : constant Token_Class := Class (Input.Read (1));
         Here      : constant Tokens.Token_Position := Tokens.Position (Input.Read);
      begin
         --
         -- Here we are at the beginning of a statement and the current
         -- token is an identifier.  We can have several cases
         --
         -- * A procedure call with parameter, e.g., foo(2);
         -- * A procedure call without parameter, e.g., foo;
         -- * An assignment to a vecto, e.g., foo(2) := 1.3;
         -- * Other assignments, i.e., foo.bar := 4; bar := 0;
         --
         -- It follows that the identifier can be followed by
         -- * A semicolon   (call, no parameters)
         -- * A dot         (assignment)
         -- * An assignment (assignment)
         -- * A parenthesis (call or assignment?)
         -- In the last case we need to look after the closed parenthesis
         -- in order to decied
         --
         --  Put_Line ("Assign or call " & Follower'Image & ", " & Image (Input.Read));

         if not (Follower in ID_Follower) then
            raise Parsing_Error
              with "Unexpected token "
              &  Class (Input.Read)'Image
              & " after IDENTIFIER"
              & " at " & Image (Here);
         end if;

         case ID_Follower (Follower) is
            when End_Of_Statement =>

               declare
                  Result : constant Code_Trees.Parsed_Code :=
                             Code_Trees.Procedure_Call (Value (Input.Next));
               begin
                  Expect_And_Eat (Input, End_Of_Statement, "END OF STATEMENT");
                  return Result;
               end;

            when Assign | Dot | Comma =>
               return Parse_Assign (Input);

            when Open_Parenthesis =>
               --
               -- This is the ambigous case.  We use back-track, so we
               -- first try the call alternative, if it does not work
               -- we backtrack to current position and try assignment
               --
               Input.Save_Position;

               declare
                  ID         : constant String := Value (Input.Read);
                  Parameters : Statement_Sequences.Sequence;
               begin
                  Input.Next;
                  Expect_And_Eat (Input, Open_Parenthesis, "OPEN 1");

                  Parameters := Parse_Expression_List (Input);

                  Expect_And_Eat (Input, Close_Parenthesis, "OPEN 2");

                  case Class (Input.Read) is
                     when End_Of_Statement | End_Of_Text =>
                        Input.Clear_Position;

                        Expect_And_Eat (Input, Class (Input.Read), "OPEN 3");

                        return Code_Trees.Procedure_Call (ID, Parameters.Dump, Here);

                     when Assign | Dot =>
                        Input.Restore_Position;

                        return Parse_Assign (Input);

                     when others =>
                        raise Parsing_Error
                          with "Unexpected token "
                          &  Class (Input.Read)'Image
                          & " after indexed identifier"
                          & " at " & Image (Here);
                  end case;

               end;
         end case;
      end Parse_Assign_Or_Call;

      function Parse_ID_List (Input : in out Scanning.Token_List)
                              return Code_Trees.Parsed_Code
      is
         In_Optional_Section : Boolean := False;
         Default             : Statement_Sequences.Sequence;
         Parameter_Names     : Code_Trees.ID_List;
      begin
         loop
            if Class (Input.Read) /= Identifier then
               --  Put_Line (">>>" & Class (Input.Read)'Image);
               raise Parsing_Error
                 with "Expected IDENTIFIER in ID list"
                 & " found " & Class (Input.Read)'Image
                 & " at " & Image (Position (Input.Read));
            end if;

            Parameter_Names.Append (ID (Value (Input.Next)));

            if Class (Input.Read) = Assign then
               Input.Next;
               Default.Append (Parse_Expression (Input));
               In_Optional_Section := True;
            else
               Default.Append (Code_Trees.Empty_Tree);

               if In_Optional_Section then
                  raise Parsing_Error
                    with "Mandatory parameter in optional parameter part"
                    & " at " & Image (Position (Input.Read));
               end if;
            end if;

            exit when Class (Input.Read) /= End_Of_Statement;

            Input.Next;
         end loop;

         return Code_Trees.Parameter_List (Parameter_Names, Default.Dump);
      end Parse_ID_List;

      function Parse_Defun (Input : in out Scanning.Token_List)
                            return Code_Trees.Parsed_Code;
      --          with Post => Code_Trees.Class (Parse_Defun'Result) = Code_Trees.Defun;

      function Parse_Defun (Input : in out Scanning.Token_List)
                            return Code_Trees.Parsed_Code
      is
         Is_Function    : constant Boolean :=
                            (case Class (Input.Read) is
                                when Kw_Procedure => False,
                                when Kw_Function  => True,
                                when others       => raise Program_Error);

         Name            : Unbounded_String;
         Parameter_Names : Code_Trees.Parsed_Code;
         Function_Body   : Code_Trees.Parsed_Code;
         Here            : constant Tokens.Token_Position := Tokens.Position (Input.Read);
      begin
         Input.Next;

         if Class (Input.Read) /= Identifier then
            raise Parsing_Error
              with "Expected IDENTIFIER in function/procedure definition"
              & " found " & Class (Input.Read)'Image
              & " at " & Image (Here);
         else
            Name := To_Unbounded_String (Value (Input.Next));
         end if;

         if Class (Input.Read) = Open_Parenthesis then
            Expect_And_Eat (Input, Open_Parenthesis);

            Parameter_Names := Parse_ID_List (Input);

            Expect_And_Eat (Input, Close_Parenthesis);
         else
            Parameter_Names := Code_Trees.Empty_Parameter_List;
         end if;

         Expect_And_Eat (Input, Kw_Is);

         Expect_And_Eat (Input, Kw_Begin);

         Function_Body := Parse_Statement_Sequence (Input);

         Expect_And_Eat (Input, Kw_End);

         if not (Class (Input.Read) = Identifier
                 and then
                 Value (Input.Read) = To_String (Name))
         then
            raise Parsing_Error
              with "end " & To_String (Name) & "; expected at " & Image (Here);
         else
            Input.Next;
         end if;

         Expect_And_Eat (Input, End_Of_Statement);

         return Code_Trees.Definition (Name           => To_String (Name),
                                       Parameter_List => Parameter_Names,
                                       Function_Body  => Function_Body,
                                       Is_Function    => Is_Function,
                                       Position       => Here);
      end Parse_Defun;


      Result : Statement_Sequences.Sequence;

      Here   : constant Token_Position := Position (Input.Read);
   begin
      --        Scanning.Dump (Input);
      --  Put_Line ("We are back!");
      loop
         --  Put_Line (Image (Input.read));
         case Class (Input.Read) is

            when Identifier =>
               --  Put_Line ("Below identifier");
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

            when Kw_Procedure | Kw_Function =>
               Result.Append (Parse_Defun (Input));

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

            when Kw_Else | Kw_Elsif | Kw_End | End_Of_Text =>
               exit;

            when Int              | Text             | Plus  | Minus       |
               Mult             | Div              | Equal |  Different  |
               Less_Than        | Greater_Than     | Less_Or_Equal       |
               Greater_Or_Equal | Assign           | Dot                 |
               Comma            | End_Of_Statement | Close_Parenthesis   |
               Kw_Then          | Kw_And           | Kw_Or               |
               Open_Parenthesis | Label_Separator  |
               Kw_When          | Kw_In            | Kw_Of               |
               Real             | Kw_Xor           | Kw_Not              |
               Kw_Begin         | Kw_Is            | Kw_Capture          |
               Kw_Mod =>

               Unexpected_Token (Class (Input.Read), End_Of_Text, Position (Input.Read));
               exit;
         end case;
      end loop;

      return Code_Trees.Statement_Sequence (Statement_Sequences.Dump (Result), Here);
   exception
      when E : Scanning.Scanning_Error =>
         raise Parsing_Error with Ada.Exceptions.Exception_Message (E);
   end Parse_Statement_Sequence;


end Protypo.Parsing;
