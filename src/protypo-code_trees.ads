
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
private
   type Parsed_Code is new Integer;

   Empty_Tree : constant Parsed_Code := 0;
end Protypo.Code_Trees;
