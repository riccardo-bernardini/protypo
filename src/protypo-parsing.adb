pragma Ada_2012;
with Protypo.Tokens;

package body Protypo.Parsing is

   ------------------------------
   -- Parse_Statement_Sequence --
   ------------------------------

   function Parse_Statement_Sequence
     (Input : in out Scanning.Token_List) return Code_Trees.Parsed_Code
   is
      use Tokens;
   begin
      while not Input.End_Of_Sequence loop
         case Input.Read is
            when Int =>
            when Real =>
            when Text =>
            when Identifier =>
            when Plus =>
            when Minus =>
            when Open_Parenthesis =>
            when Kw_If =>
            when Kw_Case =>
            when Kw_When =>
            when Kw_For =>
            when Kw_Loop =>
            when Kw_Return =>
            when Mult             |  Div             | Equal |  Different  |
                 Less_Than        | Greater_Than     | Less_Or_Equal       |
                 Greater_Or_Equal | Assign           | Dot                 |
                 Comma            | End_Of_Statement | Close_Parenthesis   |
                 Kw_Then           |
                 Kw_Elsif | Kw_Else | Kw_End | Kw_And | Kw_Or | Kw_In | Kw_Of =>
         end case;
      end loop;
   end Parse_Statement_Sequence;

end Protypo.Parsing;
