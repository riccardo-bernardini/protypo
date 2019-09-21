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
         case Class(Input.Read)	 is
            when Int | Real | Text | Plus | Minus|  Open_Parenthesis =>
               Parse_Naked_Expression;

            when Identifier =>
               Parse_Name;
               pragma Compile_Time_Warning (True, "Warning");
               raise Program_Error;

            when Kw_If =>
               Parse_Conditional;

            when Kw_Case =>
               Parse_Case;

            when Kw_For =>
               Parse_For_Loop;

            when Kw_While =>
               Parse_While_Loop;

            when Kw_Loop =>
               Parse_Loop;

            when Kw_Return =>
               Parse_Return;

            when Mult             | Div              | Equal |  Different  |
                 Less_Than        | Greater_Than     | Less_Or_Equal       |
                 Greater_Or_Equal | Assign           | Dot                 |
                 Comma            | End_Of_Statement | Close_Parenthesis   |
                 Kw_Then          | Kw_Elsif         | Kw_Else             |
                 Kw_End           | Kw_And           | Kw_Or               |
                 Kw_When          | Kw_In            | Kw_Of =>

               raise Constraint_Error;
         end case;
      end loop;
   end Parse_Statement_Sequence;

end Protypo.Parsing;
