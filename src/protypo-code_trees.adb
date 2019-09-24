pragma Ada_2012;
package body Protypo.Code_Trees is

   -----------
   -- Class --
   -----------

   function Class (X : Parsed_Code) return Non_Terminal is
   begin
      pragma Compile_Time_Warning (Standard.True, "Class unimplemented");
      return raise Program_Error with "Unimplemented function Class";
   end Class;

   ------------------
   -- If_Then_Else --
   ------------------

   function If_Then_Else
     (Condition   : Tree_Array; Then_Branch : Tree_Array;
      Else_Branch : Parsed_Code) return Parsed_Code
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "If_Then_Else unimplemented");
      return raise Program_Error with "Unimplemented function If_Then_Else";
   end If_Then_Else;

   ----------------
   -- Assignment --
   ----------------

   function Assignment
     (LHS : Tree_Array; Expression : Tree_Array) return Parsed_Code
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Assignment unimplemented");
      return raise Program_Error with "Unimplemented function Assignment";
   end Assignment;

   ------------------------
   -- Statement_Sequence --
   ------------------------

   function Statement_Sequence (Statements : Tree_Array) return Parsed_Code is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Statement_Sequence unimplemented");
      return raise Program_Error
          with "Unimplemented function Statement_Sequence";
   end Statement_Sequence;

   ----------------------
   -- Naked_Expression --
   ----------------------

   function Naked_Expression (Statements : Tree_Array) return Parsed_Code is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Naked_Expression unimplemented");
      return raise Program_Error
          with "Unimplemented function Naked_Expression";
   end Naked_Expression;

   ----------------------
   -- Binary_Operation --
   ----------------------

   function Binary_Operation
     (Left      : Parsed_Code; Right : Parsed_Code;
      Operation : Tokens.Binary_Operator) return Parsed_Code
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Binary_Operation unimplemented");
      return raise Program_Error
          with "Unimplemented function Binary_Operation";
   end Binary_Operation;

   ---------------------
   -- Unary_Operation --
   ---------------------

   function Unary_Operation
     (X : Parsed_Code; Operation : Tokens.Unary_Operator) return Parsed_Code
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Unary_Operation unimplemented");
      return raise Program_Error with "Unimplemented function Unary_Operation";
   end Unary_Operation;

   ---------------------
   -- String_Constant --
   ---------------------

   function String_Constant (Val : String) return Parsed_Code is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "String_Constant unimplemented");
      return raise Program_Error with "Unimplemented function String_Constant";
   end String_Constant;

   ----------------------
   -- Integer_Constant --
   ----------------------

   function Integer_Constant (Val : String) return Parsed_Code is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Integer_Constant unimplemented");
      return raise Program_Error
          with "Unimplemented function Integer_Constant";
   end Integer_Constant;

   --------------------
   -- Float_Constant --
   --------------------

   function Float_Constant (Val : String) return Parsed_Code is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Float_Constant unimplemented");
      return raise Program_Error with "Unimplemented function Float_Constant";
   end Float_Constant;

   ----------------
   -- Identifier --
   ----------------

   function Identifier (Id : String) return Parsed_Code is
   begin
      pragma Compile_Time_Warning (Standard.True, "Identifier unimplemented");
      return raise Program_Error with "Unimplemented function Identifier";
   end Identifier;

   -------------------
   -- Function_Call --
   -------------------

   function Function_Call
     (Function_Ref : Parsed_Code; Parameters : Tree_Array) return Parsed_Code
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Function_Call unimplemented");
      return raise Program_Error with "Unimplemented function Function_Call";
   end Function_Call;

   --------------
   -- Selector --
   --------------

   function Selector (Ref : Parsed_Code; Field : String) return Parsed_Code is
   begin
      pragma Compile_Time_Warning (Standard.True, "Selector unimplemented");
      return raise Program_Error with "Unimplemented function Selector";
   end Selector;

   -----------------
   -- Conditional --
   -----------------

   function Conditional
     (Conditions  : Tree_Array; Branches : Tree_Array;
      Else_Branch : Parsed_Code) return Parsed_Code
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Conditional unimplemented");
      return raise Program_Error with "Unimplemented function Conditional";
   end Conditional;

   ----------------
   -- Basic_Loop --
   ----------------

   function Basic_Loop (Loop_Body : Parsed_Code) return Parsed_Code is
   begin
      pragma Compile_Time_Warning (Standard.True, "Basic_Loop unimplemented");
      return raise Program_Error with "Unimplemented function Basic_Loop";
   end Basic_Loop;

   --------------
   -- For_Loop --
   --------------

   function For_Loop
     (Variable : Parsed_Code; Iterator : Parsed_Code; Loop_Body : Parsed_Code)
      return Parsed_Code
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "For_Loop unimplemented");
      return raise Program_Error with "Unimplemented function For_Loop";
   end For_Loop;

   ----------------
   -- While_Loop --
   ----------------

   function While_Loop
     (Condition : Parsed_Code; Loop_Body : Parsed_Code) return Parsed_Code
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "While_Loop unimplemented");
      return raise Program_Error with "Unimplemented function While_Loop";
   end While_Loop;

   ----------------------
   -- Return_To_Caller --
   ----------------------

   function Return_To_Caller (Values : Tree_Array) return Parsed_Code is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Return_To_Caller unimplemented");
      return raise Program_Error
          with "Unimplemented function Return_To_Caller";
   end Return_To_Caller;

end Protypo.Code_Trees;
