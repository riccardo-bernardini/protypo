with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package Protypo.Scanning is
   type Token_Class is
     (Int,
      Real,
      Text,
      Plus,
      Minus,
      Mult,
      Div,
      Dot,
      Open_Parenthesis,
      Close_Parenthesis,
      End_Of_Statement,
      Kw_If,
      Kw_Then,
      Kw_Else,
      Kw_End);

   type Token is private;

   function Class (Tk : Token) return Token_Class;
   function Value (Tk : Token) return String;

   type Token_List is tagged private;

   function Tokenize(Input : String) return Token_List;
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

end Protypo.Scanning;
