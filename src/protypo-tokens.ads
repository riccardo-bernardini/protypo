with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;


package Protypo.Tokens is
   type Token_Class is
     (Int,
      Real,
      Text,
      Plus, Minus, Mult, Div,
      Equal, Different,
      Less_Than, Greater_Than,
      Less_Or_Equal, Greater_Or_Equal,
      Assign,
      Dot,
      Open_Parenthesis,
      Close_Parenthesis,
      Comma,
      End_Of_Statement,
      Kw_If,
      Kw_Then,
      Kw_Else,
      Kw_End,
      Kw_And,
      Kw_Or);

   type Token is private;

   function Class (Tk : Token) return Token_Class;
   function Value (Tk : Token) return String;
private
   type Token is
      record
         Class : Token_Class;
         Value : Unbounded_String;
      end record;


   function Class (Tk : Token) return Token_Class
   is (Tk.Class);

   function Value (Tk : Token) return String
   is (To_String (Tk.Value));
end Protypo.Tokens;
