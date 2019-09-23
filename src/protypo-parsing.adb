pragma Ada_2012;
with Protypo.Tokens;
with Readable_Sequences.Generic_Sequences;

use Readable_Sequences;
use Tokens;
package body Protypo.Parsing is

   type Token_Mask is array (Token_Class) of Boolean;


   package Statement_Sequences is
     new Generic_Sequences (Element_Type  => Code_Trees.Parsed_Code,
                            Element_Array => Code_Trees.Tree_Array);

   ------------------------------
   -- Parse_Statement_Sequence --
   ------------------------------

   function Parse_Statement_Sequence
     (Input      : in out Scanning.Token_List;
      Valid_End  : Token_Mask) return Code_Trees.Parsed_Code
   is
      use Tokens;
      use Code_Trees;

      Result : Statement_Sequences.Sequence;
   begin
      loop
         case Class(Input.Read)	 is

            when Identifier =>
               Result.Append (Parse_Assign (Input));

            when Kw_If =>
               Result.Append (Parse_Conditional (Input));

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
