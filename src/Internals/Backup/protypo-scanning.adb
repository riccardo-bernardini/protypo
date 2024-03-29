pragma Ada_2012;

with Ada.Strings.Maps.Constants;     use Ada.Strings.Maps.Constants;
with Ada.Characters.Latin_9;         use Ada.Characters.Latin_9;
with Ada.Text_IO;                    use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Sequential_IO;
with Ada.Directories;


with Readable_Sequences.String_Sequences;
use Readable_Sequences;

with String_Sets;                    use String_Sets;

package body Protypo.Scanning is


   -----------
   -- Slurp --
   -----------

   function Slurp (Filename : String) return String is

      subtype Content is String (1 .. Integer (Ada.Directories.Size (Filename)));
      Result : Content;

      package Content_IO is new Ada.Sequential_IO (Content);
      Input  : Content_IO.File_Type;

   begin
      Content_IO.Open (File     => Input,
                       Mode     => Content_IO.In_File,
                       Name     => Filename);

      Content_IO.Read (File => Input,
                       Item => Result);

      Content_IO.Close (Input);

      return Result;
   end Slurp;


   type Error_Reason is
     (Unexpected_Short_End,
      Unexpected_Token_In_Code,
      Unexpected_Code_End,
      Bad_Float);

   -----------
   -- Error --
   -----------

   procedure Error (Reason : Error_Reason)
   is
   begin
      raise Scanning_Error with (case Reason is
                                    when Unexpected_Short_End     =>
                                      "Unexpected end of short code",
                                    when Unexpected_Token_In_Code =>
                                      "Unexpected token code",
                                    when Unexpected_Code_End      =>
                                      "Unexpected end  code",
                                    when Bad_Float                =>
                                      "Bad Float"
                                );
   end Error;



   Begin_Of_Code          : constant String := "#{";
   Short_Code_Begin       : constant Set_String := "#" & Letter_Set;
   Transparent_Comment    : constant String := "%#";
   Target_Comment         : constant String := "%";
   Template_Comment       : constant String := "#--";
   Directive_Begin_Marker : constant Character := '(';
   Directive_End_Marker   : constant Character := ')';
   Directive_Begin        : constant String := "#" & Directive_Begin_Marker;



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

   procedure Skip_Comment (Input : in out String_Sequences.Sequence) is
   begin
      while not At_End_Of_Line (Input) loop
         Input.Next;
      end loop;
   end Skip_Comment;

   ----------------------------
   -- Target_Comment_Scanner --
   ----------------------------

   procedure Target_Comment_Scanner (Buffer : in out String_Sequences.Sequence;
                                     Input  : in out String_Sequences.Sequence)
   is
   begin
      while not At_End_Of_Line (Input) loop
         Buffer.Append (Input.Read);
         Input.Next;
      end loop;
   end Target_Comment_Scanner;


   ------------------
   -- Code_Scanner --
   ------------------
   procedure  Code_Scanner (Builder : in out Tokens.Token_Builder;
                            Input   : in out String_Sequences.Sequence;
                            Result  : in out Token_List)
     with Post => Result.Length > Result.Length'Old;

   procedure  Code_Scanner (Builder : in out Tokens.Token_Builder;
                            Input   : in out String_Sequences.Sequence;
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
               Error (Bad_Float);
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
         Buffer : String_Sequences.Sequence;
      begin
         Buffer.Clear;

         loop
            if Peek_And_Eat (Input, "]") then
               exit when Input.Read /= ']';
            end if;

            Buffer.Append (Input.Next);
         end loop;

         Result.Append (Builder.Make_Token (Identifier, Consume_With_Escape_Procedure_Name));
         Result.Append (Make_Unanchored_Token (Open_Parenthesis));
         Result.Append (Make_Unanchored_Token (Text, Buffer.Dump));
         Result.Append (Make_Unanchored_Token (Close_Parenthesis));
         Result.Append (Make_Unanchored_Token (End_Of_Statement));

      end Scan_Embedded_Text;
   begin
      loop
         Skip_Spaces;
         Builder.Set_Position (Input.Position);

         exit when Peek_And_Eat (Input, "}#");

         if Peek_And_Eat (Input, "--") then
            Skip_Comment (Input);
            Builder.Clear_Position;

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
                  Put_Line ("[" & Input.Read & "]");
                  Error (Unexpected_Token_In_Code);
               end if;
            end;
         end if;
      end loop;

   exception
      when String_Sequences.Beyond_End =>
         Error (Unexpected_Code_End);
   end Code_Scanner;

   procedure  Small_Code_Scanner (Builder : in out Tokens.Token_Builder;
                                  Input   : in out String_Sequences.Sequence;
                                  Result  : in out Token_List)
     with Post => Result.Length > Result.Length'Old;

   procedure  Small_Code_Scanner (Builder : in out Tokens.Token_Builder;
                                  Input   : in out String_Sequences.Sequence;
                                  Result  : in out Token_List)
   is
      use Tokens;

      type Scanner_State is (In_ID, After_Dot);

      State   : Scanner_State := In_ID;
      ID_Name : String_Sequences.Sequence;
   begin


      while not Input.End_Of_Sequence loop
         case State is
            when In_ID =>
               if Is_In (Input.Read, Id_Charset) then
                  Id_Name.Append (Input.Read);
                  Input.Next;

               elsif Input.Read = '.' then
                  Result.Append (Builder.Make_Token (Identifier, ID_Name.Dump));
                  Result.Append (Make_Unanchored_Token (Dot));
                  ID_Name.Clear;
                  Input.Next;

                  State := After_Dot;

               else
                  exit;
               end if;

            when After_Dot =>
               if Is_In (Input.Read, Id_Charset) then
                  Id_Name.Append (Input.Read);
                  Input.Next;

                  State := In_ID;
               else
                  Error (Unexpected_Short_End);
               end if;
         end case;
      end loop;

      if State = After_Dot then
         Error (Unexpected_Short_End);
      end if;

      pragma Assert (ID_Name.Length > 0);

      Result.Append (Builder.Make_Token (Identifier, Consume_Procedure_Name));
      Result.Append (Make_Unanchored_Token (Open_Parenthesis));
      Result.Append (Make_Unanchored_Token (Identifier, ID_Name.Dump));
      Result.Append (Make_Unanchored_Token (Close_Parenthesis));
      Result.Append (Make_Unanchored_Token (End_Of_Statement));

   end Small_Code_Scanner;

   -----------------
   -- Dump_Buffer --
   -----------------

   procedure Dump_Buffer (Builder : in out Tokens.Token_Builder;
                          Buffer  : in out String_Sequences.Sequence;
                          Result  : in out Token_List)
     with
       Post =>
         Buffer.Length = 0
         and Result.Length = Result.Length'Old + (if Buffer.Length'Old > 0 then 5 else 0);


   procedure Dump_Buffer (Builder : in out Tokens.Token_Builder;
                          Buffer  : in out String_Sequences.Sequence;
                          Result  : in out Token_List)
   is
      use Tokens;

   begin
      if Buffer.Length > 0 then
         Result.Append (Builder.Make_Token (Identifier, Consume_Procedure_Name));
         Result.Append (Make_Unanchored_Token (Open_Parenthesis));
         Result.Append (Make_Unanchored_Token (Text, Buffer.Dump));
         Result.Append (Make_Unanchored_Token (Close_Parenthesis));
         Result.Append (Make_Unanchored_Token (End_Of_Statement));
         Buffer.Clear;
      end if;
   end Dump_Buffer;


   ---------------------------------
   -- Transparent_Comment_Scanner --
   ---------------------------------

   procedure Transparent_Comment_Scanner (Builder : in out Tokens.Token_Builder;
                                          Buffer  : in out String_Sequences.Sequence;
                                          Input  : in out String_Sequences.Sequence;
                                          Result : in out Token_List)
   is
   begin
      while not At_End_Of_Line (Input) loop
         if Does_It_Follow (Input, Short_Code_Begin)  then
            Dump_Buffer (Builder, Buffer, Result);
            Input.Next;
            Builder.Set_Position (Input.Position);

            Small_Code_Scanner (Builder, Input, Result);
            Builder.Set_Position(Input.Position);
         else
            Buffer.Append (Input.Read);
            Input.Next;
         end if;
      end loop;
   end Transparent_Comment_Scanner;

   procedure Process_Directive (Input    : in out String_Sequences.Sequence;
                                Result   : in out Token_List;
                                Base_Dir : String)
   is
      Buffer            : String_Sequences.Sequence;
      Parenthesis_Level : Natural := 1;

   begin
      while Parenthesis_Level > 0 loop
         if Input.Read = Directive_Begin_Marker then
            Parenthesis_Level := Parenthesis_Level + 1;

            Buffer.Append (Input.Next);

         elsif Input.Read = Directive_End_Marker then
            if Input.Read (1) = Directive_End_Marker then
               Buffer.Append (Directive_End_Marker);
               Input.Next (2);

            else
               Parenthesis_Level := Parenthesis_Level - 1;

               if Parenthesis_Level > 0 then
                  Buffer.Append (Input.Read);
               end if;

               Input.Next;
            end if;

         else
            Buffer.Append (Input.Next);
         end if;
      end loop;

      declare
         use Ada.Strings.Fixed;
         use Ada.Strings;
         use Ada.Characters.Handling;

         S : constant String := Trim (Buffer.Dump, Left);

         End_Directive_Pos : constant Natural := Index (Source  => S,
                                                        Pattern => " ");

         Directive : constant String := To_Lower (if End_Directive_Pos = 0 then
                                                     S
                                                  else
                                                     S (S'First .. End_Directive_Pos - 1));

         Parameter : constant String := Trim ((if End_Directive_Pos < S'Last then
                                                 S (End_Directive_Pos + 1 .. S'Last)
                                              else
                                                 ""), Left);
      begin
         if Directive = "include" then
            declare
               Included : constant Token_List :=
                            Tokenize (Slurp (Parameter), Base_Dir);
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

   function Tokenize (Template : String;
                      Base_Dir : String) return Token_List is
      use Tokens;

      Builder : Tokens.Token_Builder;

      Result  : Token_List := Token_Sequences.Create (Builder.Make_Token (End_Of_Text));
      Input   : String_Sequences.Sequence := String_Sequences.Create (Template);
      Buffer  : String_Sequences.Sequence;

   begin
      while not Input.End_Of_Sequence loop
         Builder.Set_Position (Input.Position);

         if Does_It_Follow (Input, Begin_Of_Code) then
            Dump_Buffer (Builder, Buffer, Result);
            Input.Next (Begin_Of_Code'Length);

            Code_Scanner (Builder, Input, Result);

         elsif Does_It_Follow (Input, Directive_Begin) then
            Dump_Buffer (Builder, Buffer, Result);
            Input.Next (Directive_Begin'Length);

            Process_Directive (Input, Result, Base_Dir);

--           elsif Does_It_Follow (Input, Short_Code_Begin)  then
--              Dump_Buffer (Builder, Buffer, Result);
--              Input.Next;
--
--              Small_Code_Scanner (Input, Result);

         elsif Does_It_Follow (Input, Transparent_Comment) then
            Buffer.Append (Target_Comment);
            Input.Next (Transparent_Comment'Length);

            Transparent_Comment_Scanner (Builder, Buffer, Input, Result);

         elsif Does_It_Follow (Input, Target_Comment) then
            Buffer.Append (Target_Comment);
            Input.Next (Target_Comment'Length);

            Target_Comment_Scanner (Buffer, Input);

         elsif Does_It_Follow (Input, Template_Comment) then
            Skip_Comment (Input);

         else
            Buffer.Append (Input.Read);
            Input.Next;
         end if;
      end loop;

      Dump_Buffer (Builder, Buffer, Result);

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
