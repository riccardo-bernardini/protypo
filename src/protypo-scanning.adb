pragma Ada_2012;
with Ada.Strings.Maps.Constants;

with Readable_Sequences.String_Sequences;

use Readable_Sequences;
use Ada.Strings.Maps;
use Ada.Strings.Maps.Constants;

with Ada.Characters.Latin_9; use Ada.Characters.Latin_9;
with Ada.Text_IO; use Ada.Text_IO;

package body Protypo.Scanning is
   type Error_Reason is
     (Unexpected_Short_End);

   -----------
   -- Error --
   -----------

   procedure Error (Reason : Error_Reason)
   is
   begin
      raise Scanning_Error with (case Reason is
                                    when Unexpected_Short_End =>
                                      "Unexpected end of short code");
   end Error;

   type Set_String is array (Positive range <>) of Character_Set;

   -------------------
   -- To_Set_String --
   -------------------

   function To_Set_String (X : String) return Set_String
   is
      Result : Set_String (X'Range);
   begin
      for K in X'Range loop
         Result (K) := To_Set (X (K));
      end loop;

      return Result;
   end To_Set_String;

   function "&" (X : String;  Y : Character_Set) return Set_String
   is (To_Set_String (X) & Set_String'(1 => Y));

   Begin_Of_Code       : constant String := "#{";
   Short_Code_Begin    : constant Set_String := "#" & Letter_Set;
   Transparent_Comment : constant String := "%#";
   Target_Comment      : constant String := "%";
   Template_Comment    : constant String := "#--";

   ----------
   -- Peek --
   ----------

   function Peek (Where : String_Sequences.Sequence;
                  What  : Set_String)
                  return Boolean
   is
   begin
      if Where.Remaining < What'Length then
         return False;
      end if;

      for K in What'Range loop
         if not Is_In (Where.Read (K - What'First), What (K)) then
            return False;
         end if;
      end loop;

      return True;
   end Peek;


   ----------
   -- Peek --
   ----------

   function Peek (Where : String_Sequences.Sequence;
                  What  : String)
                  return Boolean
   is (Peek (Where, To_Set_String (What)));

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
   procedure  Code_Scanner (Input  : in out String_Sequences.Sequence;
                            Result : in out Token_List)
     with Post => Result.Length > Result.Length'Old;

   procedure  Code_Scanner (Input  : in out String_Sequences.Sequence;
                            Result : in out Token_List)
   is
      pragma Unreferenced (Input);
   begin
      pragma Compile_Time_Warning (True, "Code_Scanner unimplemented");
      raise Program_Error with "Unimplemented";
   end Code_Scanner;

   procedure  Small_Code_Scanner (Input  : in out String_Sequences.Sequence;
                                  Result : in out Token_List)
     with Post => Result.Length > Result.Length'Old;

   procedure  Small_Code_Scanner (Input  : in out String_Sequences.Sequence;
                                  Result : in out Token_List)
   is
      use Tokens;

      Id_Charset : constant Character_Set := Alphanumeric_Set or To_Set ('_');

      type Scanner_State is (In_ID, After_Dot);

      State : Scanner_State := In_ID;
      ID_Name : String_Sequences.Sequence;
   begin
      while not Input.End_Of_Sequence loop
         case State is
            when In_ID =>
               if Is_In (Input.Read, Id_Charset) then
                  Id_Name.Append (Input.Read);
                  Input.Next;

               elsif Input.Read = '.' then
                  Result.Append (Make_Token (Identifier, ID_Name.Dump));
                  Result.Append (Make_Token (Dot));
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
      Result.Append (Make_Token (Identifier, ID_Name.Dump));
      Result.Append (Make_Token (End_Of_Statement));

   end Small_Code_Scanner;

   -----------------
   -- Dump_Buffer --
   -----------------

   procedure Dump_Buffer (Buffer : in out String_Sequences.Sequence;
                          Result : in out Token_List)
     with
       Post =>
         Buffer.Length = 0
         and Result.Length = Result.Length'Old + (if Buffer.Length'Old > 0 then 2 else 0);


   procedure Dump_Buffer (Buffer : in out String_Sequences.Sequence;
                          Result : in out Token_List)
   is
      use Tokens;
   begin
      if Buffer.Length > 0 then
         Result.Append (Make_Token (Text, Buffer.Dump));
         Result.Append (Make_Token (End_Of_Statement));
         Buffer.Clear;
      end if;
   end Dump_Buffer;


   ---------------------------------
   -- Transparent_Comment_Scanner --
   ---------------------------------

   procedure Transparent_Comment_Scanner (Buffer : in out String_Sequences.Sequence;
                                          Input  : in out String_Sequences.Sequence;
                                          Result : in out Token_List)
   is
   begin
      while not At_End_Of_Line (Input) loop
         if Peek (Input, Short_Code_Begin)  then
            Dump_Buffer (Buffer, Result);
            Input.Next;

            Small_Code_Scanner (Input, Result);
         else
            Buffer.Append (Input.Read);
            Input.Next;
         end if;
      end loop;
   end Transparent_Comment_Scanner;


   --------------
   -- Tokenize --
   --------------

   function Tokenize (Template : String) return Token_List is

      Result : Token_List;
      Input  : String_Sequences.Sequence := String_Sequences.Create (Template);
      Buffer : String_Sequences.Sequence;

   begin
      while not Input.End_Of_Sequence loop
         if Peek (Input, Begin_Of_Code) then
            Dump_Buffer (Buffer, Result);
            Input.Next (Begin_Of_Code'Length);

            Code_Scanner (Input, Result);

         elsif Peek (Input, Short_Code_Begin)  then
            Dump_Buffer (Buffer, Result);
            Input.Next;

            Small_Code_Scanner (Input, Result);

         elsif Peek (Input, Transparent_Comment) then
            Buffer.Append (Target_Comment);
            Input.Next (Transparent_Comment'Length);

            Transparent_Comment_Scanner (Buffer, Input, Result);

         elsif Peek (Input, Target_Comment) then
            Buffer.Append (Target_Comment);
            Input.Next (Target_Comment'Length);

            Target_Comment_Scanner (Buffer, Input);

         elsif Peek (Input, Template_Comment) then
            Skip_Comment (Input);

         else
            Buffer.Append (Input.Read);
            Input.Next;
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
