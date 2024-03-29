pragma Ada_2012;
with Ada.Calendar.Formatting;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Gnat.Regpat;

with Protypo.Api.Engine_Values.Range_Iterators;
with Protypo.Api.Engine_Values.Handlers;
with Protypo.Api.Engine_Values.Engine_Value_Array_Wrappers;
with Protypo.Api.Interpreters;
with Protypo.Api.Callback_Utilities;  use Protypo.Api.Callback_Utilities;

with Protypo.Code_Trees.Interpreter.Statements;
with Protypo.Code_Trees.Interpreter.Expressions;
with Protypo.Code_Trees.Interpreter.String_Interpolation_Handlers;
with Protypo.Code_Trees.Interpreter.Consumer_Handlers;

with Protypo.Scanning;
with Protypo.Parsing;

with Readable_Sequences.String_Sequences; use Readable_Sequences;




pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Characters.Latin_1;


with Gnat.Regexp;

with Tokenize;
with Protypo.Match_Data_Wrappers;

package body Protypo.Code_Trees.Interpreter is
   use Ada.Strings;


   ---------------
   -- Do_Escape --
   ---------------

   function Do_Escape (Status : Interpreter_Access;
                       Input  : String)
                       return String
   is
      function To_String (X : Engine_Value) return String
      is (case X.Class is
             when Int    => Get_Integer (X)'Image,
             when Real   => Get_Float (X)'Image,
             when Text   => Get_String (X),
             when others => raise Constraint_Error);

      --------------------
      -- Parse_And_Eval --
      --------------------

      function Parse_And_Eval (Status : Interpreter_Access;
                               Input  : String)
                               return Engine_Value
      is
         Tk   : Scanning.Token_List :=
                  Scanning.Tokenize (Api.Interpreters.Template_Type ("#{" & Input & "}#"), "");

         Code : constant Code_Trees.Parsed_Code :=
                  Parsing.Parse_Expression (Tk);
      begin
         --  Scanning.Dump (Tk);
         --  Code_Trees.Dump (Code);


         return Expressions.Eval_Scalar (Status, Code.Pt);
      end Parse_And_Eval;

      type Automata_State is (Reading_Text, Reading_Expression);

      Eof           : constant Character := Character'Val (0);
      Seq           : String_Sequences.Sequence := String_Sequences.Create (Input, Eof);
      Current_State : Automata_State := Reading_Text;

      Result : String_Sequences.Sequence := String_Sequences.Empty_Sequence;
      Expr   : String_Sequences.Sequence := String_Sequences.Empty_Sequence;
   begin
      while not Seq.End_Of_Sequence loop
         case Current_State is
            when Reading_Text =>

               if Seq.Read = '#' and Seq.Read (1) /= '#' then
                  Seq.Next;
                  Expr.Clear;

                  Current_State := Reading_Expression;
               elsif Seq.Read = '#' and Seq.Read (1) = '#' then
                  Seq.Next (2);

                  Result.Append ('#');
               else
                  Result.Append (Seq.Next);
               end if;

            when Reading_Expression =>

               if Seq.Read = '#' then
                  Seq.Next;

                  Current_State := Reading_Text;

                  --                    Put_Line ("<" & Expr.Dump & ">");
                  Result.Append (To_String (Parse_And_Eval (Status, Expr.Dump)));
               else
                  Expr.Append (Seq.Next);
               end if;
         end case;
      end loop;

      return Result.Dump;
   end Do_Escape;


   function To_String (X : Engine_Value) return String
   is (case X.Class is
          when Int    => Fixed.Trim (Get_Integer (X)'Image, Both),
          when Real   => Fixed.Trim (Get_Float (X)'Image, Both),
          when Text   => Get_String (X),
          when others => X.Class'Image);

   function To_Stderr (Parameters : Engine_Value_Vectors.Vector)
                       return Engine_Value_Vectors.Vector
   is
      use type Ada.Containers.Count_Type;
   begin
      if Parameters.Length /= 1 then
         raise Run_Time_Error with "Debug needs 1 parameter";
      end if;

      Put_Line (File => Standard_Error,
                Item => "DEBUG>> " & To_String (Parameters (Parameters.First)));

      return Engine_Value_Vectors.Empty_Vector;
   end To_Stderr;

   function Stringify (Parameters : Engine_Value_Vectors.Vector)
                       return Engine_Value_Vectors.Vector
   is
      use type Ada.Containers.Count_Type;
   begin
      if Parameters.Length /= 1 then
         raise Run_Time_Error with "image needs 1 parameter";
      end if;

      return Engine_Value_Vectors.To_Vector
        (Create (To_String (Parameters (Parameters.First))), 1);
   end Stringify;

   function Date_Callback  (Parameters : Engine_Value_Vectors.Vector)
                            return Engine_Value_Vectors.Vector
   is

      use type Ada.Containers.Count_Type;
      use Ada.Calendar.Formatting;
      use Ada.Calendar;
   begin
      if Parameters.Length /= 0 then
         raise Run_Time_Error with "date wants no parameter";
      end if;

      return Engine_Value_Vectors.To_Vector (Create (Image (Clock)), 1);

   end Date_Callback;

   --------------------
   -- Range_Callback --
   --------------------

   function Range_Callback (Parameters : Engine_Value_Vectors.Vector)
                            return Engine_Value_Vectors.Vector
   is
      use type Ada.Containers.Count_Type;

   begin
      if Parameters.Length /= 2 then
         raise Run_Time_Error with "range needs 2 parameters";
      end if;

      declare
         First : constant Integer := Get_Integer (Parameters (Parameters.First_Index));
         Last  : constant Integer := Get_Integer (Parameters (Parameters.First_Index + 1));
         Val   : constant Engine_Value := Handlers.Create (Range_Iterators.Create (First, Last));
      begin
         return Engine_Value_Vectors.To_Vector (Val, 1);
      end;
   end Range_Callback;




   function Substring_Callback  (Parameters : Engine_Value_Vectors.Vector)
                                 return Engine_Value_Vectors.Vector
   is

   begin
      if not Match_Signature (Parameters => Parameters,
                              Signature  => (Text, Int, Int))
      then
         raise Run_Time_Error with "substr needs 1 string + 2 int";
      end if;

      declare
         Item : constant String :=
                  Get_String (Parameters (Parameters.First_Index));

         First : Integer :=
                   Get_Integer (Parameters (Parameters.First_Index + 1));

         Last : Integer :=
                  Get_Integer (Parameters (Parameters.First_Index + 2));
      begin
         if First < 0 then
            First := Item'Last + First + 1;
         end if;

         if Last < 0 then
            Last := Item'Last + Last + 1;
         end if;

         if First < Item'First or First > Item'Last then
            raise Run_Time_Error
              with
                "First index in substring " & First'Image
                & " is outside valid range "
              & Item'First'Image & ".." & Item'Last'Image;
         end if;

         if Last < Item'First or Last > Item'Last then
            raise Run_Time_Error
              with
                "Last index in substring " & Last'Image
                & " is outside valid range "
              & Item'First'Image & ".." & Item'Last'Image;
         end if;

         return Engine_Value_Vectors.To_Vector (Create (Item (First .. Last)), 1);
      end;
   end Substring_Callback;

   function Split_Callback  (Parameters : Engine_Value_Vectors.Vector)
                             return Engine_Value_Vectors.Vector
   is

   begin
      if not Match_Signature (Parameters => Parameters,
                              Signature  => (Text, Text))
      then
         raise Run_Time_Error with "glob needs 2 string parameters";
      end if;

      declare
         Item      : constant String :=
                       Get_String (Parameters (Parameters.First_Index));

         Separator : constant String :=
                       Get_String (Parameters (Parameters.First_Index + 1));
      begin
         if Separator'Length /= 1 then
            raise Run_Time_Error
              with
                "Split separator should be one character long, found "
                & "'" & Separator & "'";
         end if;

         declare
            use Tokenize;
            use Protypo.Api.Engine_Values.Engine_Value_Array_Wrappers;

            Tokens : constant Token_Array :=
                       Split (Item, Separator (Separator'First));

            Result : Engine_Value_Vectors.Vector;
         begin
            for Tk of Tokens loop
               Result.Append (Create (Tk));
            end loop;

            return Engine_Value_Vectors.To_Vector
              (Handlers.Create (Make_Wrapper (Result)), 1);
         end;
      end;
   end Split_Callback;

   function Regexp_Callback (Parameters : Engine_Value_Vectors.Vector)
                             return Engine_Value_Vectors.Vector
   is
      use Gnat.Regpat;
   begin
      if not Match_Signature (Parameters => Parameters,
                              Signature  => (Text, Text))
      then
         raise Run_Time_Error with "regexp needs 2 string parameters";
      end if;

      declare
         use Engine_Value_Vectors;
         use Protypo.Match_Data_Wrappers;

         Item    : constant String := Get_Parameter (Parameters, 1);
         Regexp  : constant String := Get_Parameter (Parameters, 2);
         Matcher : constant Pattern_Matcher := Compile (Regexp);
         Matched : Match_Array (0 .. Paren_Count (matcher));
      begin
         Match (Matcher, Item, Matched);

         return To_Vector (Wrap (Matched, Item), 1);
      end;
   exception
      when E : Gnat.Regpat.Expression_Error =>
         raise Run_Time_Error
           with
             "Bad Regexp '" & Get_Parameter (Parameters, 2) & "' :"
           & Ada.Exceptions.Exception_Message (E);
   end Regexp_Callback;

   function Glob_Callback (Parameters : Engine_Value_Vectors.Vector)
                           return Engine_Value_Vectors.Vector
   is
      use Gnat.Regexp;
   begin
      if not Match_Signature (Parameters => Parameters,
                              Signature  => (Text, Text))
      then
         raise Run_Time_Error with "glob needs 2 string parameters";
      end if;


      declare
         Item    : constant String := Get_String (Parameters (Parameters.First_Index));
         Pattern : constant String := Get_String (Parameters (Parameters.First_Index + 1));
         Re      : constant Regexp := Compile (Pattern, Glob => True);
         Val     : constant Engine_Value := Create (Match (Item, Re));
      begin
         return Engine_Value_Vectors.To_Vector (Val, 1);
      end;
   end Glob_Callback;


   procedure Update_Consumer (Inter    : Interpreter_Access;
                              Consumer : Api.Consumers.Consumer_Access)
   is
      use Consumer_Handlers;
      use Api.Symbols.Protypo_Tables;
      use Ada.Characters.Latin_1;
   begin
      Update
        (Pos          => Inter.Consumer_Without_Escape_Cursor,
         New_Value    =>
           Handlers.Create (Create (Consumer    => Consumer,
                                    With_Escape => False,
                                    End_Of_Line => Null_Unbounded_String,
                                    Status      => Inter)));
      Update
        (Pos          => Inter.Consumer_With_Escape_Cursor,
         New_Value    =>
           Handlers.Create (Create (Consumer    => Consumer,
                                    With_Escape => True,
                                    End_Of_Line => To_Unbounded_String ("" & Lf),
                                    Status      => Inter)));

   end Update_Consumer;

   ---------
   -- Run --
   ---------

   procedure Run
     (Program      : Parsed_Code;
      Symbol_Table : Api.Symbols.Table;
      Consumer     : Api.Consumers.Consumer_Access)
   is
      use Api.Symbols;

      procedure Add_Builtin_Values (Table    : in out Api.Symbols.Table;
                                    Inter    : Interpreter_Access)
      is
         use Consumer_Handlers;
         use Api.Symbols.Protypo_Tables;
         use Ada.Characters.Latin_1;
      begin
         Table.Create
           (Name            => Scanning.Consume_Procedure_Name,
            Initial_Value   =>
              Handlers.Create (Create (Consumer    => Consumer,
                                       With_Escape => False,
                                       End_Of_Line => Null_Unbounded_String,
                                       Status      => Inter)),
            Position        => Inter.Consumer_Without_Escape_Cursor);


         Table.Create
           (Name            => Scanning.Consume_With_Escape_Procedure_Name,
            Initial_Value   =>
              Handlers.Create (Create (Consumer    => Consumer,
                                       With_Escape => True,
                                       End_Of_Line => To_Unbounded_String ("" & Lf),
                                       Status      => Inter)),
            Position        => Inter.Consumer_With_Escape_Cursor);

         Table.Create
           (Name          => "true",
            Initial_Value => Create (True));

         Table.Create
           (Name          => "false",
            Initial_Value => Create (False));

         Table.Create
           (Name          => "range",
            Initial_Value => Handlers.Create (Range_Callback'Access, 2));

         Table.Create
           (Name          => "glob",
            Initial_Value => Handlers.Create (Glob_Callback'Access, 2));

         Table.Create
           (Name          => "match",
            Initial_Value => Handlers.Create (Regexp_Callback'Access, 2));

         Table.Create
           (Name          => "split",
            Initial_Value => Handlers.Create (Split_Callback'Access, 2));

         Table.Create
           (Name          => "substr",
            Initial_Value => Handlers.Create (Substring_Callback'Access, 3));

         Table.Create
           (Name          => "now",
            Initial_Value => Handlers.Create (Date_Callback'Access, 0));

         Table.Create
           (Name          => "image",
            Initial_Value => Handlers.Create (Stringify'Access, 1));

         Table.Create
           (Name          => "debug",
            Initial_Value => Handlers.Create (To_Stderr'Access, 1));

         Table.Create
           (Name          => "expand",
            Initial_Value =>
              Handlers.Create (String_Interpolation_Handlers.Create (Inter)));
      end Add_Builtin_Values;

      Interpreter : constant Interpreter_Access :=
                      new Interpreter_Type'
                        (Break                          => No_Break,
                         Symbol_Table                   => Copy_Globals (Symbol_Table),
                         Saved_Consumers                => Consumer_Stacks.Empty_List,
                         Consumer_Without_Escape_Cursor => Api.Symbols.Protypo_Tables.No_Element,
                         Consumer_With_Escape_Cursor    => Api.Symbols.Protypo_Tables.No_Element);
   begin
      --        Code_Trees.Dump (Program);
      Api.Symbols.Protypo_Tables.Set_Printer (To_String'Access);

      Add_Builtin_Values (Interpreter.Symbol_Table, Interpreter);

      Statements.Run (Interpreter, Program.Pt);

      if Interpreter.Break /= No_Break  then
         raise Program_Error;
      end if;
   end Run;


   procedure Push_Consumer (Interpreter : Interpreter_Access;
                            Consumer    : Api.Consumers.Consumer_Access)
   is
      use Api.Symbols.Protypo_Tables;
      use Consumer_Handlers;

      Pos : constant Cursor := Interpreter.Symbol_Table.Find (Scanning.Consume_Procedure_Name);
   begin
      if Pos = No_Element then
         raise Program_Error with "Consumer not found?!?";
      end if;

      declare
         Fun      : constant Handlers.Function_Interface_Access :=
                      Handlers.Get_Function (Value (Pos));

         Callback : constant Consumer_Callback := Consumer_Callback (Fun.all);
      begin
         Interpreter.Saved_Consumers.Prepend (User_Consumer (Callback));
      end;

      Update_Consumer (Interpreter, Consumer);
   end Push_Consumer;

   procedure Pop_Consumer (Interpreter : Interpreter_Access)
   is
      Old_Consumer : constant Api.Consumers.Consumer_Access :=
                       Interpreter.Saved_Consumers.First_Element;
   begin
      Interpreter.Saved_Consumers.Delete_First;

      Update_Consumer (Interpreter, Old_Consumer);
   end Pop_Consumer;


end Protypo.Code_Trees.Interpreter;


--     function "+" (X : Engine_Value_Vectors.Vector)
--                   return Engine_Value
--           with
--                 Pre => X.Length = 1;

--     function "+" (X : Engine_Value_Vectors.Vector)  return Engine_Value
--     is (X.First_Element);

--     function "+" (X : Engine_Value)  return Engine_Value_Vectors.Vector
--     is
--        Result : Engine_Value_Vectors.Vector;
--     begin
--        Result.Append (X);
--        return Result;
--     end "+";

--     function "+" (X : Engine_Value)  return Engine_Value_Vectors.Vector
--     is (To_Array (+X));
--
--     function "+" (X : Engine_Value_Vectors.Vector)  return Engine_Value
--     is (if X'Length = 1 then
--            X (X'First)
--         else
--            raise Constraint_Error);

