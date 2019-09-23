with Protypo.Tokens;

package Protypo.Code_Trees is
   type Non_Terminal is
     (
      If_Block,
      For_Block,
      Loop_Block,
      While_Block,
      Return_Block,
      Assignment,
      Stat_Sequence,
      List_Of_Expr,
      List_Of_Names,
      Naked,
      Binary_Op,
      Unary_Op,
      Cost,
      Selector,
      Call,
      Identifier
     );

   subtype Expr is Non_Terminal range Binary_Op .. Identifier;
   subtype Name is Non_Terminal range Selector .. Identifier;

   type Parsed_Code is private;

   Empty_Tree : constant Parsed_Code;

   type Tree_Array is array (Positive range <>) of Parsed_Code;


   Empty_Tree_Array : constant Tree_Array;

   function Class (X : Parsed_Code) return Non_Terminal;

   function If_Then_Else (Condition   : Tree_Array;
                          Then_Branch : Tree_Array;
                          Else_Branch : Parsed_Code)
                          return Parsed_Code
     with
       Pre =>
         Condition'Length = Then_Branch'Length
         and (for all Cond of Condition => Class (Cond) in Expr)
         and (for all B of Then_Branch => Class (B) in Stat_Sequence),
         Post =>
           Class (If_Then_Else'Result) = If_Block;

   function Assignment (LHS        : Tree_Array;
                        Expression : Tree_Array)
                        return Parsed_Code
     with
       Pre => Lhs'Length = Expression'Length
       and (for all Item of Lhs => Class (Item) in Name)
       and (for all Item of Expression => Class (Item) in Expr),
       Post => Class (Assignment'Result) = Assignment;

   function Statement_Sequence (Statements : Tree_Array)
                                return Parsed_Code
     with
       Post => Class (Statement_Sequence'Result) = Stat_Sequence;

   function Naked_Expression (Statements : Tree_Array)
                              return Parsed_Code
     with
       Post => Class (Naked_Expression'Result) = Naked;

   function Binary_Operation (Left      : Parsed_Code;
                              Right     : Parsed_Code;
                              Operation : Tokens.Binary_Operator)
                              return Parsed_Code
     with
       Post => Class (Binary_Operation'Result) = Binary_Op;

   function Unary_Operation (X         : Parsed_Code;
                             Operation : Tokens.Unary_Operator)
                             return Parsed_Code
     with
       Post => Class (Unary_Operation'Result) = Unary_Op;


   function String_Constant (Val : String) return Parsed_Code
     with
       Post => Class (String_Constant'Result) = Cost;

   function Integer_Constant (Val : String) return Parsed_Code
     with
       Post => Class (Integer_Constant'Result) = Cost;

   function Float_Constant (Val : String) return Parsed_Code
     with
       Post => Class (Float_Constant'Result) = Cost;


   function Identifier (Id : String) return Parsed_Code
     with
       Post => Class (Identifier'Result) = Identifier;

   function Function_Call (Function_Ref : Parsed_Code;
                           Parameters   : Tree_Array)
                           return Parsed_Code
     with
       Post => Class (Function_Call'Result) = Call;


   function Selector (Ref   : Parsed_Code;
                      Field : String)
                      return Parsed_Code
     with
       Post => Class (Selector'Result) = Selector;


   function Conditional (Conditions  : Tree_Array;
                         Branches    : Tree_Array;
                         Else_Branch : Parsed_Code)
                         return Parsed_Code
     with
       Post => Class (Conditional'Result) = If_Block;


   function Basic_Loop (Loop_Body : Parsed_Code)
                        return Parsed_Code
     with
       Post => Class (Basic_Loop'Result) = Loop_Block;


   function For_Loop (Variable  : Parsed_Code;
                      Iterator  : Parsed_Code;
                      Loop_Body : Parsed_Code)
                      return Parsed_Code
     with
       Post => Class (For_Loop'Result) = For_Block,
     Pre =>
       Class (Variable) = Identifier
     and
       Class (Loop_Body) = Loop_Block;


   function While_Loop (Condition : Parsed_Code;
                        Loop_Body : Parsed_Code)
                        return Parsed_Code
     with
       Pre => Class (Loop_Body) = Loop_Block  and (Class (Condition) in Expr),
     Post => Class (While_Loop'Result) = While_Block;



   function Return_To_Caller (Values : Tree_Array)
                              return Parsed_Code
     with
       Pre => (for all V of Values => Class (V) in Expr),
       Post => Class (Return_To_Caller'Result) = Return_Block;

private
   type Parsed_Code is new Integer;

   Empty_Tree : constant Parsed_Code := 0;

   Empty_Tree_Array : constant Tree_Array (2 .. 1) := (others => <>);

end Protypo.Code_Trees;
