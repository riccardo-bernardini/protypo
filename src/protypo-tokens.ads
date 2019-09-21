with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;


package Protypo.Tokens is
   type Token_Class is
     (Int,
      Real,
      Text,
      Identifier,
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

   subtype Valued_Token is Token_Class range Int .. Identifier;
   subtype Unvalued_Token is Token_Class range Plus .. Kw_Or;

   type Token is private;

   function Make_Token (Class : Valued_Token;
                        Value : String)
                        return Token
     with Pre => Value /= "";

   function Make_Token (Class : Unvalued_Token)
                        return Token;

   function Class (Tk : Token) return Token_Class;

   function Value (Tk : Token) return String
     with Pre => Class (Tk) in Valued_Token;

   function Image (Tk : Token) return String;
private
   type Token is
      record
         Class : Token_Class;
         Value : Unbounded_String;
      end record
     with Dynamic_Predicate => ((Class in Unvalued_Token) = (Value = Null_Unbounded_String));



   function Make_Token (Class : Valued_Token;
                        Value : String)
                        return Token
   is (Token'(Class => Class,
              Value => To_Unbounded_String (Value)));

   function Make_Token (Class : Unvalued_Token)
                        return Token
   is (Token'(Class => Class,
              Value => Null_Unbounded_String));

   function Class (Tk : Token) return Token_Class
   is (Tk.Class);

   function Value (Tk : Token) return String
   is (To_String (Tk.Value));


   function Image (Tk : Token) return String
   is (Token_Class'Image (Tk.Class)
       & " "
       & (if Tk.Class in Valued_Token then
             "(" & To_String (Tk.Value) & ")"
          else
             ""));
end Protypo.Tokens;
