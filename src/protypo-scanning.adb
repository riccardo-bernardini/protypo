pragma Ada_2012;
with Readable_Sequences.String_Sequences;

use Readable_Sequences;

package body Protypo.Scanning is

   --------------
   -- Tokenize --
   --------------

   function Tokenize (Input : String) return Token_List is
      Result : Token_List;
      Data   : String_Sequences.Sequence := String_Sequences.Create (Input);
      Buffer : String_Sequences.Sequence;

      procedure Dump_Buffer is
         use Tokens;
      begin
         Result.Append (New_Token (Str, Buffer.Dump));
         Result.Append (New_Token (End_Of_Statement));
         Buffer.Clear;
      end Dump_Buffer;
   begin
      while not Data.End_Of_Sequence loop
         if Data.Is_At ("#{") then
            Dump_Buffer;
            Data.Next (2);

            Code_Scanner (Data, Result);

         elsif Data.Is_At ("#" & Letter_Set) then
            Dump_Buffer;
            Data.Next;

            Small_Code_Scanner (Data, Result);

         elsif Data.Is_At ("%#") then
            Buffer.Append ('%');
            Data.Next;

            Transparent_Comment_Scanner (Buffer, Data, Result);

         elsif Data.Is_At ("%") then
            Buffer.Append ('%');
            Data.Next;

            Target_Comment_Scanner (Buffer, Data, Result);

         elsif Data.Is_At ("#--") then
            Skip_Comment (Data);

         else
            Buffer.Append (Data.Read);
            Data.Next;
         end if;
      end loop;
   end Tokenize;

end Protypo.Scanning;
