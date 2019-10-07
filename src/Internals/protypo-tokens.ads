--  with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Readable_Sequences.String_Sequences;
use Readable_Sequences;
package Protypo.Tokens is
   use Ada.Strings.Unbounded;

   subtype Token_Position is Readable_Sequences.String_Sequences.Position_Type;

   function Image (X : Token_Position) return String;

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
      Label_Separator,
      End_Of_Statement,
      Kw_If,
      Kw_Then,
      Kw_Elsif,
      Kw_Else,
      Kw_Case,
      Kw_When,
      Kw_For,
      Kw_While,
      Kw_Loop,
      Kw_Exit,
      Kw_Function,
      Kw_Procedure,
      Kw_Begin,
      Kw_Return,
      Kw_End,
      Kw_And,
      Kw_Or,
      Kw_Xor,
      Kw_Not,
      Kw_In,
      Kw_Is,
      Kw_Of,
      End_Of_Text);


   subtype Valued_Token     is Token_Class     range Int .. Identifier;
   subtype Unvalued_Token   is Token_Class     range Plus .. End_Of_Text;
   subtype Not_Keyword      is Unvalued_Token  range Plus .. End_Of_Statement;
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

   type Token_Builder is tagged private;

   function Is_Position_Set (Builder : Token_Builder) return Boolean;

   procedure Set_Position (Builder  : in out Token_Builder;
                           Position : Token_Position)
     with
       Pre => not Builder.Is_Position_Set,
       Post => Builder.Is_Position_Set;

   procedure Clear_Position (Builder  : in out Token_Builder)
     with
       Pre => Builder.Is_Position_Set,
       Post => not Builder.Is_Position_Set;

   function Make_Token (Builder : in out Token_Builder;
                        Class   : Valued_Token;
                        Value   : String)
                        return Token
     with
       Pre => Value /= "" and Builder.Is_Position_Set,
     Post => not Builder.Is_Position_Set;

   function Make_Token (Builder : in out Token_Builder;
                        Class   : Unvalued_Token)
                        return Token
     with
       Pre => Builder.Is_Position_Set,
       Post => not Builder.Is_Position_Set;

   function Make_Unanchored_Token (Class   : Valued_Token;
                                   Value   : String)
                                   return Token;

   function Make_Unanchored_Token (Class : Unvalued_Token)
                                   return Token;


   function Class (Tk : Token) return Token_Class;

   function Value (Tk : Token) return String
     with Pre => Class (Tk) in Valued_Token;

   function Image (Tk : Token) return String;

   function Position (Tk : Token) return Token_Position;
   function Line (Tk : Token) return Positive;
   function Char (Tk : Token) return Positive;
private
   use type String_Sequences.Position_Type;

   type Token_Builder is tagged
      record
         Has_Position : Boolean := False;
         Position     : Token_Position;
      end record;

   type Token is
      record
         Class    : Token_Class;
         Value    : Unbounded_String;
         Position : Token_Position;
      end record
     with Dynamic_Predicate => ((Class in Unvalued_Token) = (Value = Null_Unbounded_String));


   function Make_Unanchored_Token (Class : Unvalued_Token)
                                   return Token
   is ((Class   => Class,
        Value    => Null_Unbounded_String,
        Position => String_Sequences.No_Position));


   function Make_Unanchored_Token (Class   : Valued_Token;
                                   Value   : String)
                                   return Token
   is ((Class   => Class,
        Value    => To_Unbounded_String (Value),
        Position => String_Sequences.No_Position));


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

   function Position (Tk : Token) return Token_Position
   is (Tk.Position);

   function Line (Tk : Token) return Positive
   is (String_Sequences.Line (Tk.Position));

   function Char (Tk : Token) return Positive
   is (String_Sequences.Char (Tk.Position));


   function Image (X : Token_Position) return String
   is (if X = String_Sequences.No_Position then
          "<unknown>"
       else
          "Line "
       & String_Sequences.Line (X)'Image
       & ", column: "
       & String_Sequences.Char (X)'Image);
end Protypo.Tokens;
