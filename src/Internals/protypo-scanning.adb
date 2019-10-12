pragma Ada_2012;

with Ada.Strings.Maps.Constants;     use Ada.Strings.Maps.Constants;
with Ada.Characters.Latin_9;         use Ada.Characters.Latin_9;
with Ada.Text_IO;                    use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;


with Readable_Sequences.String_Sequences;
use Readable_Sequences;

with String_Sets;                    use String_Sets;

package body Protypo.Scanning is
   use Ada.Strings.Unbounded;
   use Ada.Strings.Maps;

   ------------------------
   -- Parenthesis_String --
   ------------------------

   function Parenthesis_String
     (Input  : in out String_Sequences.Sequence;
      Open   : Character;
      Close  : Character;
      Escape : Character)
      return String
   is
      Buffer            : String_Sequences.Sequence;
      Parenthesis_Level : Natural := 1;
   begin
      while Parenthesis_Level > 0 loop
         if Input.End_Of_Sequence then
            raise Scanning_Error with "Unexpected EOF";
         end if;

         if Input.Read = Escape then
            Input.Next;
            Buffer.Append (Input.Next);

         elsif Input.Read = Open then
            Parenthesis_Level := Parenthesis_Level + 1;

            Buffer.Append (Input.Next);

         elsif Input.Read = Close then
            Parenthesis_Level := Parenthesis_Level - 1;

            if Parenthesis_Level > 0 then
               Buffer.Append (Input.Read);
            end if;

            Input.Next;

         else
            Buffer.Append (Input.Next);
         end if;
      end loop;

      return Buffer.Dump;
   end Parenthesis_String;


   type Error_Reason is
     (Unexpected_Token_In_Code,
      Unexpected_Code_End,
      Bad_Float);

   -----------
   -- Error --
   -----------

   procedure Error (Reason   : Error_Reason;
                    Position : String_Sequences.Position_Type)
   is
   begin
      raise Scanning_Error with (case Reason is
                                    when Unexpected_Token_In_Code =>
                                      "Unexpected token code",
                                    when Unexpected_Code_End      =>
                                      "Unexpected end  code",
                                    when Bad_Float                =>
                                      "Bad Float"
                                ) & " at " & Tokens.Image (Position);
   end Error;



   Escape           : constant Character := '#';
   Begin_Of_Code    : constant String := Escape & "{";
   Template_Comment : constant String := Escape & "--";
   Directive_Begin  : constant Character := '(';
   Directive_Marker : constant String := Escape & Directive_Begin;

   --     Short_Code_Begin       : constant Set_String := "#" & Letter_Set;
   --     Transparent_Comment    : constant String := "%#";
   --     Target_Comment         : constant String := "%";



   function Does_It_Follow (Where    : String_Sequences.Sequence;
                            Pattern  : Set_String)
                            return Boolean;
   -- Return true if a string matching Pattern is found at the current
   -- position

   function Does_It_Follow (Where : String_Sequences.Sequence;
                            What  : String)
                            return Boolean;
   -- Syntactic sugar for when the set contains only one string

   function Peek_And_Eat (Where : in out String_Sequences.Sequence;
                          What  : String)
                          return Boolean;
   -- If What is at the current position, "eat it" and return true,
   -- otherwise leave the position unchanged and return false.
   -- Very convenient


   function Does_It_Follow (Where    : String_Sequences.Sequence;
                            Pattern  : Set_String)
                            return Boolean
   is (Match (Where.Dump (From => Where.Current_Position), Pattern));



   --------------------
   -- Does_It_Follow --
   --------------------

   function Does_It_Follow (Where : String_Sequences.Sequence;
                            What  : String)
                            return Boolean
   is (Does_It_Follow (Where, To_Set_String (What)));
   -- Syntactic sugar for when the set contains only one string

   ------------------
   -- Peek_And_Eat --
   ------------------

   function Peek_And_Eat (Where : in out String_Sequences.Sequence;
                          What  : String)
                          return Boolean
   is
   begin
      if Does_It_Follow (Where, What) then
         Where.Next (What'Length);
         return True;
      else
         return False;
      end if;
   end Peek_And_Eat;

   --------------------
   -- At_End_Of_Line --
   --------------------

   EOL_Markers : constant Ada.Strings.Maps.Character_Set :=
                   Ada.Strings.Maps.To_Set ("" & CR & LF);

   function At_End_Of_Line (Input : String_Sequences.Sequence) return Boolean
   is (Input.End_Of_Sequence
       or else Ada.Strings.Maps.Is_In (Input.Read, EOL_Markers));

   ------------------
   -- Skip_Comment --
   ------------------

   procedure Skip_To_End_Of_Line (Input : in out String_Sequences.Sequence) is
   begin
      while not At_End_Of_Line (Input) loop
         Input.Next;
      end loop;
   end Skip_To_End_Of_Line;

   ------------------
   -- Code_Scanner --
   ------------------
   procedure  Code_Scanner (Input   : in out String_Sequences.Sequence;
                            Result  : in out Token_List)
     with Post => Result.Length > Result.Length'Old;

   procedure  Code_Scanner (Input   : in out String_Sequences.Sequence;
                            Result  : in out Token_List)
   is
      use Tokens;

      function "+" (X : String) return Unbounded_String
                    renames To_Unbounded_String;

      Simple_Tokens : constant array (Not_Keyword) of Unbounded_String :=
                        (
                         Plus              => +"+",
                         Minus             => +"-",
                         Mult              => +"*",
                         Div               => +"/",
                         Equal             => +"=",
                         Different         => +"/=",
                         Less_Than         => +"<",
                         Greater_Than      => +">",
                         Less_Or_Equal     => +"<=",
                         Greater_Or_Equal  => +">=",
                         Assign            => +":=",
                         Dot               => +".",
                         Open_Parenthesis  => +"(",
                         Close_Parenthesis => +")",
                         Tokens.Comma      => +",",
                         Label_Separator   => +":",
                         End_Of_Statement  => +";"
                        );

      Keywords : constant array (Keyword_Tokens) of Unbounded_String :=
                   (Kw_If        => +"if",
                    Kw_Then      => +"then",
                    Kw_Elsif     => +"elsif",
                    Kw_Else      => +"else",
                    Kw_Case      => +"case",
                    Kw_When      => +"when",
                    Kw_For       => +"for",
                    Kw_Loop      => +"loop",
                    Kw_While     => +"while",
                    Kw_Return    => +"return",
                    Kw_Function  => +"function",
                    Kw_Procedure => +"procedure",
                    Kw_Begin     => +"begin",
                    Kw_Exit      => +"exit",
                    Kw_End       => +"end",
                    Kw_And       => +"and",
                    Kw_Or        => +"or",
                    Kw_Xor       => +"xor",
                    Kw_Not       => +"not",
                    Kw_In        => +"in",
                    Kw_Is        => +"is",
                    Kw_Of        => +"of");

      Builder : Tokens.Token_Builder;

      procedure Skip_Spaces
        with
          Pre => not Builder.Is_Position_Set,
          Post => not Builder.Is_Position_Set;

      procedure Scan_Identifier
        with
          Pre => Builder.Is_Position_Set,
          Post => not Builder.Is_Position_Set;

      procedure Scan_Number
        with
          Pre => Builder.Is_Position_Set,
          Post => not Builder.Is_Position_Set;

      procedure Scan_Text
        with
          Pre => Builder.Is_Position_Set,
          Post => not Builder.Is_Position_Set;

      procedure Scan_Embedded_Text
        with
          Pre => Builder.Is_Position_Set,
          Post => not Builder.Is_Position_Set;


      procedure Skip_Spaces is
         Whitespace : constant Character_Set := To_Set (" " & HT & LF & CR);
      begin
         while (not Input.End_Of_Sequence) and then Is_In (Input.Read, Whitespace)  loop
            Input.Next;
         end loop;
      end Skip_Spaces;

      procedure Scan_Identifier is
         Id : String_Sequences.Sequence;
      begin
         while Is_In (Input.Read, Id_Charset) loop
            Id.Append (Input.Read);
            Input.Next;
         end loop;

         for Tk in Keywords'Range loop
            if To_String (Keywords (Tk)) = Id.Dump then
               Result.Append (Builder.Make_Token (Tk));
               return;
            end if;
         end loop;

         Result.Append (Builder.Make_Token (Identifier, Id.Dump));
      end Scan_Identifier;

      procedure Scan_Number is
         Buffer : String_Sequences.Sequence;
      begin

         while Is_In (Input.Read, Decimal_Digit_Set) loop
            Buffer.Append (Input.Next);
         end loop;

         if Input.Read /= '.' then
            Result.Append (Builder.Make_Token (Int, Buffer.Dump));
            return;
         end if;

         Buffer.Append (Input.Next);

         while Is_In (Input.Read, Decimal_Digit_Set) loop
            Buffer.Append (Input.Next);
         end loop;

         if Input.Read = 'e' or Input.Read = 'E' then
            Buffer.Append (Input.Next);

            if Input.Read = '+' or Input.Read = '-' then
               Buffer.Append (Input.Next);
            end if;

            if not Is_In (Input.Read, Decimal_Digit_Set) then
               Error (Bad_Float, String_Sequences.Position (Input));
            end if;

            while Is_In (Input.Read, Decimal_Digit_Set) loop
               Buffer.Append (Input.Next);
            end loop;
         end if;

         Result.Append (Builder.Make_Token (Real, Buffer.Dump));
      end Scan_Number;

      procedure Scan_Text is
         Buffer : String_Sequences.Sequence;
      begin
         Buffer.Clear;

         loop
            if Peek_And_Eat (Input, """") then
               if Input.Read /= '"' then
                  Result.Append (Builder.Make_Token (Text, Buffer.Dump));
                  return;
               end if;
            end if;

            Buffer.Append (Input.Next);
         end loop;
      end Scan_Text;


      procedure Scan_Embedded_Text is
         Content : constant String := Parenthesis_String (Input  => Input,
                                                          Open   => '[',
                                                          Close  => ']',
                                                          Escape => '\');
      begin
         --           Buffer.Clear;
         --
         --           loop
         --              if Peek_And_Eat (Input, "]") then
         --                 exit when Input.Read /= ']';
         --              end if;
         --
         --              Buffer.Append (Input.Next);
         --           end loop;

         Result.Append (Builder.Make_Token (Identifier, String(Consume_With_Escape_Procedure_Name)));
         Result.Append (Make_Unanchored_Token (Open_Parenthesis));
         Result.Append (Make_Unanchored_Token (Text, Content));
         Result.Append (Make_Unanchored_Token (Close_Parenthesis));
         Result.Append (Make_Unanchored_Token (End_Of_Statement));

      end Scan_Embedded_Text;
   begin
      loop
         Skip_Spaces;
         Builder.Set_Position (Input.Position);

         exit when Peek_And_Eat (Input, "}#");

         if Peek_And_Eat (Input, "--") then
            Builder.Clear_Position;
            Skip_To_End_Of_Line (Input);

         elsif Is_In (Input.Read, Begin_Id_Set) then
            Scan_Identifier;

         elsif Is_In (Input.Read, Decimal_Digit_Set) then
            Scan_Number;

         elsif Peek_And_Eat (Input, """") then
            Scan_Text;

         elsif Peek_And_Eat (Input, "[") then
            Scan_Embedded_Text;

         else
            declare
               Best_Match_Len : Natural := 0;
               Best_Match     : Not_Keyword;
               This_Match_Len : Natural;
            begin
               for Tk in Simple_Tokens'Range loop
                  if Does_It_Follow (Input, To_String (Simple_Tokens (Tk))) then

                     This_Match_Len := Length (Simple_Tokens (Tk));

                     if  This_Match_Len = Best_Match_Len then
                        -- This should never happen
                        raise Program_Error;

                     elsif This_Match_Len > Best_Match_Len then
                        Best_Match_Len := This_Match_Len;
                        Best_Match := Tk;
                     end if;
                  end if;
               end loop;

               if Best_Match_Len > 0 then
                  Result.Append (Builder.Make_Token (Best_Match));
                  Input.Next (Best_Match_Len);
               else
--                    Put_Line ("[" & Input.Read & "]"
--                              & String_Sequences.Line (Input.Position)'Image
--                              & String_Sequences.Char (Input.Position)'Image);
                  Error (Unexpected_Token_In_Code, String_Sequences.Position(Input));
               end if;
            end;
         end if;

         pragma Assert (not Builder.Is_Position_Set);
      end loop;

   exception
      when String_Sequences.Beyond_End =>
         Error (Unexpected_Code_End, String_Sequences.Position(Input));
   end Code_Scanner;


   -----------------
   -- Dump_Buffer --
   -----------------

   procedure Dump_Buffer (Buffer  : in out String_Sequences.Sequence;
                          Result  : in out Token_List)
     with
       Post =>
         Buffer.Length = 0
         and Result.Length = Result.Length'Old + (if Buffer.Length'Old > 0 then 5 else 0);


   procedure Dump_Buffer (Buffer  : in out String_Sequences.Sequence;
                          Result  : in out Token_List)
   is
      use Tokens;

   begin
      if Buffer.Length > 0 then
         Result.Append (Make_Unanchored_Token (Identifier, String(Consume_Procedure_Name)));
         Result.Append (Make_Unanchored_Token (Open_Parenthesis));
         Result.Append (Make_Unanchored_Token (Text, Buffer.Dump));
         Result.Append (Make_Unanchored_Token (Close_Parenthesis));
         Result.Append (Make_Unanchored_Token (End_Of_Statement));
         Buffer.Clear;
      end if;
   end Dump_Buffer;


   procedure Process_Directive (Input    : in out String_Sequences.Sequence;
                                Result   : in out Token_List;
                                Base_Dir : String)
   is

   begin

      declare
         use Ada.Strings.Fixed;
         use Ada.Strings;
         use Ada.Characters.Handling;

         Raw : constant String := Parenthesis_String (Input  => Input,
                                                      Open   => '(',
                                                      Close  => ')',
                                                      Escape => '\');

         Trimmed : constant String := Trim (Raw, Left);

         End_Directive_Pos : constant Natural := Index (Source  => Trimmed,
                                                        Pattern => " ");

         Directive : constant String :=
                       To_Lower (if (End_Directive_Pos = 0) then
                                    Trimmed
                                 else
                                    Trimmed (Trimmed'First .. End_Directive_Pos - 1));

         Parameter : constant String :=
                       Trim ((if End_Directive_Pos < Trimmed'Last then
                                Trimmed (End_Directive_Pos + 1 .. Trimmed'Last)
                             else
                                ""), Left);
      begin
         if Directive = "include" then
            declare
               Included : constant Token_List :=
                            Tokenize (Api.Interpreters.Slurp (Parameter), Base_Dir);
            begin
               Result.Append (Included);
            end;

         else
            Put_Line (Standard_Error, "Directive '" & Directive & "' unknown");
         end if;
      end;
   end Process_Directive;


   --------------
   -- Tokenize --
   --------------

   function Tokenize (Template : Protypo.Api.Interpreters.Template_Type;
                      Base_Dir : String) return Token_List is
      use Tokens;


      Result  : Token_List := Token_Sequences.Create (Make_Unanchored_Token (End_Of_Text));
      Input   : String_Sequences.Sequence := String_Sequences.Create (String (Template));
      Buffer  : String_Sequences.Sequence;

      procedure Handle_Escape
        with Pre => Input.Read = Escape and Input.Remaining > 1;

      procedure Handle_Escape is
      begin
         if Does_It_Follow (Input, Begin_Of_Code) then
            Dump_Buffer (Buffer, Result);
            Input.Next (Begin_Of_Code'Length);

            Code_Scanner (Input, Result);

         elsif Does_It_Follow (Input, Directive_Marker) then
            Dump_Buffer (Buffer, Result);
            Input.Next (Directive_Marker'Length);

            Process_Directive (Input, Result, Base_Dir);


         elsif Does_It_Follow (Input, Template_Comment) then
            Skip_To_End_Of_Line (Input);

         else
            Buffer.Append (Input.Next);
            Buffer.Append (Input.Next);
         end if;
      end Handle_Escape;

   begin
      while not Input.End_Of_Sequence loop
         if Input.Read = Escape and Input.Remaining > 1 then
            Handle_Escape;
         else
            Buffer.Append (Input.Next);
         end if;
      end loop;

      Dump_Buffer (Buffer, Result);

      return Result;
   end Tokenize;


   ----------
   -- Dump --
   ----------

   procedure Dump (Item : Token_List)
   is
      use Tokens;

      procedure Print (X : Token) is
      begin
         Put_Line (Image (X));
      end Print;
   begin
      Item.Process (Print'Access);
   end Dump;
end Protypo.Scanning;
