with Protypo.Tokens;

package Protypo.Code_Trees is
   type Parsed_Code is private;

   Empty_Tree : constant Parsed_Code;

   type Tree_Array is array (Positive range <>) of Parsed_Code;

   function If_Then_Else (Condition   : Tree_Array;
                          Then_Branch : Tree_Array;
                          Else_Branch : Parsed_Code)
                          return Parsed_Code
     with
       Pre => Condition'Length = Then_Branch'Length;

   function Assignment (LHS        : Tree_Array;
                        Expression : Tree_Array)
                        return Parsed_Code;

   function Statement_Sequence (Statements : Tree_Array)
                                return Parsed_Code;

   function Naked_Expression (Statements : Tree_Array)
                              return Parsed_Code;

   function Binary_Operation (Left      : Parsed_Code;
                              Right     : Parsed_Code;
                              Operation : Tokens.Binary_Operator)
                              return Parsed_Code;

   function Unary_Operation (X         : Parsed_Code;
                             Operation : Tokens.Unary_Operator)
                             return Parsed_Code;

   function String_Constant (Val : String) return Parsed_Code;
   function Integer_Constant (Val : String) return Parsed_Code;
   function Float_Constant (Val : String) return Parsed_Code;

   function Identifier (Id : String) return Parsed_Code;
   function Function_Call (Function_Ref : Parsed_Code;
                           Parameters   : Tree_Array)
                           return Parsed_Code;

   function Selector (Ref   : Parsed_Code;
                      Field : String)
                      return Parsed_Code;

   function Conditional (Conditions : Tree_Array;
                         Branches   : Tree_Array;
                         Else_Branch : Parsed_Code)
                         return Parsed_Code;
private
   type Parsed_Code is new Integer;

   Empty_Tree : constant Parsed_Code := 0;
end Protypo.Code_Trees;
