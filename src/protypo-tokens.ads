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
      Open_Naked,
      Close_Naked,
      Kw_If,
      Kw_Then,
      Kw_Elsif,
      Kw_Else,
      Kw_Case,
      Kw_When,
      Kw_For,
      Kw_While,
      Kw_Loop,
      Kw_Return,
      Kw_End,
      Kw_And,
      Kw_Or,
      Kw_Xor,
      Kw_Not,
      Kw_In,
      Kw_Of,
      End_Of_Text);


   subtype Valued_Token     is Token_Class     range Int .. Identifier;
   subtype Unvalued_Token   is Token_Class     range Plus .. End_Of_Text;
   subtype Not_Keyword      is Unvalued_Token  range Plus .. Close_Naked;
   subtype Keyword_Tokens   is Unvalued_Token  range Kw_If .. Kw_Of;
   subtype Logical_Operator is Unvalued_Token  range Kw_And .. Kw_Xor;
   subtype Comp_Operator    is Unvalued_Token range Equal .. Greater_Or_Equal;
   subtype Numeric_Operator is Unvalued_Token  range Plus .. Div;

   subtype Unary_Operator   is Unvalued_Token
     with Static_Predicate =>
       Unary_Operator in Plus .. Minus | Kw_Not;

   subtype Binary_Operator  is Unvalued_Token
     with Static_Predicate =>
       Binary_Operator in Numeric_Operator | Comp_Operator | Logical_Operator;

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
