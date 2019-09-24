pragma Ada_2012;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body Protypo.Code_Trees is

   function To_Expression_Vector (X : Tree_Array)
                                  return Node_Vectors.Vector
   is
      Result : Node_Vectors.Vector;
   begin
      for Item of X loop
         if not (Item.Pt.Class in Expression) then
            raise Program_Error;
         end if;

         Result.Append (Item.Pt);
      end loop;

      return Result;
   end To_Expression_Vector;

   -----------
   -- Class --
   -----------

   function Class (X : Parsed_Code) return Non_Terminal is
   begin
      return X.Pt.Class;
   end Class;


   ------------------
   -- If_Then_Else --
   ------------------

   function If_Then_Else
     (Conditions    : Tree_Array;
      Then_Branches : Tree_Array;
      Else_Branch   : Parsed_Code)
      return Parsed_Code
   is
      Result : constant Node_Access := new Node (If_Block);
   begin
      if Conditions'Length /= Then_Branches'Length then
         raise Program_Error;
      end if;

      for Cond of Conditions loop
         Result.Conditions.Append (Cond.Pt);
      end loop;

      for Branch of Then_Branches loop
         Result.Branches.Append (Branch.Pt);
      end loop;

      Result.Else_Branch := Else_Branch.Pt;

      return (Pt => Result);
   end If_Then_Else;

   ----------------
   -- Assignment --
   ----------------

   function Assignment
     (LHS   : Tree_Array;
      Value : Tree_Array)
      return Parsed_Code
   is
      Result : constant Node_Access := new Node (Assignment);
   begin
      for Lvalue of Lhs loop
         if not (Lvalue.Pt.Class in Name) then
            raise Program_Error;
         end if;

         Result.Lhs.Append (Lvalue.Pt);
      end loop;

      for Rvalue of Value loop
         if not (Rvalue.Pt.Class in Expression) then
            raise Program_Error;
         end if;

         Result.Rvalues.Append (Rvalue.Pt);
      end loop;

      return (Pt => Result);
   end Assignment;

   ------------------------
   -- Statement_Sequence --
   ------------------------

   function Statement_Sequence
     (Statements : Tree_Array)
      return Parsed_Code
   is
      Result : constant Node_Access := new Node (Statement_Sequence);
   begin
      for Statement of Statements loop
         if not (Statement.Pt.Class in Statement_Classes) then
            raise Program_Error;
         end if;

         Result.Statements.Append (Statement.Pt);
      end loop;

      return (Pt => Result);
   end Statement_Sequence;

   ----------------------
   -- Naked_Expression --
   ----------------------

   function Naked_Expression
     (Statements : Tree_Array)
      return Parsed_Code
   is
      Result : constant Node_Access := new Node (Statement_Sequence);
   begin
      for Statement of Statements loop
         if not (Statement.Pt.Class in Expression) then
            raise Program_Error;
         end if;

         Result.Naked_Values.Append (Statement.Pt);
      end loop;

      return (Pt => Result);
   end Naked_Expression;

   ----------------------
   -- Binary_Operation --
   ----------------------

   function Binary_Operation
     (Left      : Parsed_Code;
      Right     : Parsed_Code;
      Operation : Tokens.Binary_Operator)
      return Parsed_Code
   is
      Result : constant Node_Access := new Node'(Class    => Binary_Op,
                                                 Left     => Left.Pt,
                                                 Right    => Right.Pt,
                                                 Operator => Operation);
   begin
      if not ((Left.Pt.Class in Expression) and (Right.Pt.Class in Expression)) then
         raise Program_Error;
      end if;

      return (Pt => Result);
   end Binary_Operation;

   ---------------------
   -- Unary_Operation --
   ---------------------

   function Unary_Operation
     (X         : Parsed_Code;
      Operation : Tokens.Unary_Operator)
      return Parsed_Code
   is
      Result : constant Node_Access := new Node'(Class    => Unary_Op,
                                                 Operand  => X.Pt,
                                                 Uni_Op   => Operation);
   begin
      if not (X.Pt.Class in Expression) then
         raise Program_Error;
      end if;

      return (Pt => Result);
   end Unary_Operation;

   ---------------------
   -- String_Constant --
   ---------------------

   function String_Constant
     (Val : String)
      return Parsed_Code
   is
      Result : constant Node_Access := new Node'(Class    => Text_Constant,
                                                 S        => To_Unbounded_String (Val));
   begin
      return (Pt => Result);
   end String_Constant;

   ----------------------
   -- Integer_Constant --
   ----------------------

   function Integer_Constant
     (Val : String)
      return Parsed_Code
   is
      Result : constant Node_Access := new Node'(Class => Int_Constant,
                                                 N     => Integer'Value (Val));
   begin
      return (Pt => Result);
   end Integer_Constant;

   --------------------
   -- Float_Constant --
   --------------------

   function Float_Constant
     (Val : String)
      return Parsed_Code
   is
      Result : constant Node_Access := new Node'(Class => Real_Constant,
                                                 X     => Float'Value (Val));
   begin
      return (Pt => Result);
   end Float_Constant;

   ----------------
   -- Identifier --
   ----------------

   function Identifier
     (Id : String)
      return Parsed_Code
   is
      Result : constant Node_Access := new Node'(Class    => Identifier,
                                                 ID_Value => To_Unbounded_String (Id));
   begin
      return (Pt => Result);
   end Identifier;

   ------------------
   -- Indexed_Name --
   ------------------

   function Indexed_Name
     (Function_Ref : Parsed_Code;
      Parameters   : Tree_Array)
      return Parsed_Code
   is
      Result : constant Node_Access :=
                 new Node'(Class       => Indexed,
                           Indexed_Var => Function_Ref.Pt,
                           Indexes     => To_Expression_Vector (Parameters));
   begin
      if not (Result.Indexed_Var.Class in Name) then
         raise Program_Error;
      end if;

      return (Pt => Result);
   end Indexed_Name;

   --------------------
   -- Procedure_Call --
   --------------------

   function Procedure_Call
     (Procedure_Name : Parsed_Code)
      return Parsed_Code
   is
      Result : constant Node_Access :=
                 new Node'(Class       => Procedure_Call,
                           Name        => Procedure_Name.Pt);
   begin
      if not (Result.Name.Class in Name) then
         raise Program_Error;
      end if;

      return (Pt => Result);
   end Procedure_Call;

   --------------
   -- Selector --
   --------------

   function Selector
     (Ref   : Parsed_Code;
      Field : String)
      return Parsed_Code
   is
      Result : constant Node_Access :=
                 new Node'(Class       => Selected,
                           Record_Var  => Ref.Pt,
                           Field_Name  => To_Unbounded_String (Field));
   begin
      if not (Result.Indexed_Var.Class in Name) then
         raise Program_Error;
      end if;

      return (Pt => Result);
   end Selector;

   ---------------
   -- Loop_Exit --
   ---------------

   function Loop_Exit
     (Label     : String)
      return Parsed_Code
   is
      Result : constant Node_Access :=
                 new Node'(Class      => Exit_Statement,
                           Loop_Label => To_Unbounded_String (Label));
   begin
      return (Pt => Result);
   end Loop_Exit;

   ----------------
   -- Basic_Loop --
   ----------------

   function Basic_Loop
     (Loop_Body : Parsed_Code;
      Label     : String)
      return Parsed_Code
   is
      Result : constant Node_Access := new Node'(Class      => Loop_Block,
                                                 Loop_Body  => <>,
                                                 Labl       => To_Unbounded_String (Label));
   begin
      if Loop_Body.Pt.Class /= Statement_Sequence then
         raise Program_Error;
      else
         Result.Loop_Body := Loop_Body.Pt.Statements;
      end if;

      return (Pt => Result);
   end Basic_Loop;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (X : Parsed_Code) return Node_Access
   is
   begin
      if not (X.Pt.Class in Name) then
         raise Program_Error;
      end if;

      return X.Pt;
   end Get_Name;

   --------------------
   -- Get_expression --
   --------------------

   function Get_Expression (X : Parsed_Code) return Node_Access
   is
   begin
      if not (X.Pt.Class in Expression) then
         raise Program_Error;
      end if;

      return X.Pt;
   end Get_Expression;


   --------------
   -- For_Loop --
   --------------

   function For_Loop
     (Variable  : Parsed_Code;
      Iterator  : Parsed_Code;
      Loop_Body : Parsed_Code)
      return Parsed_Code
   is
      Result : constant Node_Access := new Node'(Class      => For_Block,
                                                 Loop_Body  => <>,
                                                 Labl       => <>,
                                                 Variable   => Get_Name (Variable),
                                                 Iterator   => Get_Expression (Iterator));
   begin
      if Loop_Body.Pt.Class /= Loop_Block then
         raise Program_Error;
      else
         Result.Loop_Body := Loop_Body.Pt.Loop_Body;
         Result.Labl := Loop_Body.Pt.Labl;
      end if;

      return (Pt => Result);
   end For_Loop;

   ----------------
   -- While_Loop --
   ----------------

   function While_Loop
     (Condition : Parsed_Code;
      Loop_Body : Parsed_Code)
      return Parsed_Code
   is
      Result : constant Node_Access :=
                 new Node'(Class      => While_Block,
                           Loop_Body  => <>,
                           Labl       => <>,
                           Condition  => Get_Expression (Condition));
   begin
      if Loop_Body.Pt.Class /= Loop_Block then
         raise Program_Error;
      else
         Result.Loop_Body := Loop_Body.Pt.Loop_Body;
         Result.Labl := Loop_Body.Pt.Labl;
      end if;

      return (Pt => Result);
   end While_Loop;

   ----------------------
   -- Return_To_Caller --
   ----------------------

   function Return_To_Caller
     (Values : Tree_Array)
      return Parsed_Code
   is
   begin
      return (Pt => new Node'(Class         => Return_Statement,
                              Return_Values => To_Expression_Vector (Values)));
   end Return_To_Caller;

   ------------
   -- Delete --
   ------------

   procedure Delete (Code : in out Parsed_Code)
   is
   begin
      Delete (Code.Pt);
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (Item : in out Node_Vectors.Vector)
   is
   begin
      for Pos in Item.Iterate loop
         Delete (Item (Pos));
      end loop;
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (Item : in out Node_Access)
   is
   begin
      case Item.Class is
         when Statement_Sequence =>
            Delete (Item.Statements);

         when Naked =>
            Delete (Item.Naked_Values);

         when Assignment =>
            Delete (Item.Lhs);
            Delete (Item.Rvalues);

         when Return_Statement =>
            Delete (Item.Return_Values);

         when Procedure_Call =>
            Delete (Item.Name);

         when Exit_Statement =>
            null;

         when If_Block =>
            Delete (Item.Conditions);
            Delete (Item.Branches);
            Delete (Item.Else_Branch);

         when List_Of_Names =>
            Delete (Item.Names);

         when List_Of_Expressions =>
            Delete (Item.Exprs);

         when Binary_Op =>
            Delete (Item.Left);
            Delete (Item.Right);

         when Unary_Op =>
            Delete (Item.Operand);

         when Int_Constant | Real_Constant |  Text_Constant | Identifier =>
            null;

         when Selected =>
            Delete (Item.Record_Var);

         when Indexed =>
            Delete (Item.Indexed_Var);
            Delete (Item.Indexes);


         when Loop_Block =>
            Delete (Item.Loop_Body);


         when For_Block =>
            Delete (Item.Loop_Body);
            Delete (Item.Variable);
            Delete (Item.Iterator);

         when While_Block =>
            Delete (Item.Loop_Body);
      end case;

      Free (Item);
   end Delete;

   procedure Dump (Item  : Node_Access;
                   Level : Natural;
                   Label : String := "");

   ----------
   -- Dump --
   ----------

   procedure Dump (Item  : Node_Vectors.Vector;
                   Level : Natural;
                   Label : String := "")
   is
   begin
      for El of Item loop
         Dump (El, Level, Label);
      end loop;
   end Dump;


   ----------
   -- Dump --
   ----------

   procedure Dump (Item  : Node_Access;
                   Level : Natural;
                   Label : String := "")
   is
      use Ada.Strings.Fixed;

      function Tabbing (N : Natural) return String
      is ((N * 3) * " ");
   begin
      if Label /= "" then
         Put_Line (Tabbing (Level) &  "[" & Label & "]");
      end if;

      Put_Line (Tabbing (Level) & Item.Class'Image);

      case Item.Class is
         when Statement_Sequence =>
            Dump (Item.Statements, Level + 1);

         when Naked =>
            Dump (Item.Naked_Values, Level + 1);

         when Assignment =>
            Dump (Item.Lhs, Level + 1, "LHS");
            Dump (Item.Rvalues, Level + 1, "RHS");

         when Return_Statement =>
            Dump (Item.Return_Values, Level + 1);

         when Procedure_Call =>
            Dump (Item.Name, Level + 1, "procedure");

         when Exit_Statement =>
            null;

         when If_Block =>
            Dump (Item.Conditions, Level + 1, "conditions");
            Dump (Item.Branches, Level + 1, "branches");
            Dump (Item.Else_Branch, Level + 1, "else branch");

         when List_Of_Names =>
            Dump (Item.Names, Level + 1);

         when List_Of_Expressions =>
            Dump (Item.Exprs, Level + 1);

         when Binary_Op =>
            Put_Line (Tabbing (Level) & Item.Operator'Image);
            Dump (Item.Left, Level + 1, "left");
            Dump (Item.Right, Level + 1, "right");

         when Unary_Op =>
            Put_Line (Tabbing (Level) & Item.Uni_Op'Image);
            Dump (Item.Operand, Level + 1);

         when Int_Constant =>
            Put_Line (Tabbing(Level) & Item.N'Image);

         when real_Constant =>
            Put_Line (Tabbing(Level) & Item.X'Image);

         when  Text_Constant  =>
            Put_Line (Tabbing(Level) & """" & To_String (Item.S) & """");

         when Identifier =>
            Put_Line (Tabbing(Level) &  "<" & To_String (Item.ID_Value) & ">");

         when Selected =>
            Dump (Item.Record_Var, Level + 1);
            Put_Line (Tabbing (Level) & To_String (Item.Field_Name));

         when Indexed =>
            Dump (Item.Indexed_Var, Level + 1);
            Dump (Item.Indexes, Level + 1, "index");


         when Loop_Block =>
            Dump (Item.Loop_Body, Level + 1);


         when For_Block =>
            Dump (Item.Loop_Body, Level + 1);
            Dump (Item.Variable, Level + 1);
            Dump (Item.Iterator, Level + 1);

         when While_Block =>
            Dump (Item.Loop_Body, Level + 1);
            Dump (Item.Condition, Level + 1);
      end case;

      if Label /= "" then
         Put_Line (Tabbing (Level) &  "[/" & Label & "]");
      end if;
   end Dump;

   procedure Dump (Code : Parsed_Code)
   is
   begin
      Put_Line ("QQQ");
      Dump (Code.Pt, 0);
   end Dump;

end Protypo.Code_Trees;
