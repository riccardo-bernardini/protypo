pragma Ada_2012;
with Protypo.Scanning;
with Protypo.Parsing;
with Protypo.Code_Trees.Interpreter;
with Protypo.API.Consumers.File_Writer;

with Ada.Directories;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_IO; use Ada.Text_IO;

package body Protypo.API.Interpreter is
   use Ada.Finalization;
   use Ada;

   procedure Dump (X : Compiled_Code)
   is
   begin
      Code_Trees.Dump (X.Code);
   end Dump;

   procedure Bye (X : in out Compiled_Code)
   is
   begin
      Code_Trees.Delete (X.Code);
   end Bye;

   -------------
   -- Compile --
   -------------

   function Compile (Program  : String;
                     Base_Dir : String := "") return Compiled_Code is
      Tokens : Scanning.Token_List :=
                 Scanning.Tokenize (Template => Program,
                                    Base_Dir => (if Base_Dir /= "" then
                                                    Base_Dir
                                                 else
                                                    Directories.Current_Directory));
   begin
      return Compiled_Code'(Limited_Controlled with
                              Code => Parsing.Parse_Statement_Sequence (Tokens));
   end Compile;


   -------------
   -- Compile --
   -------------

   procedure Compile (Target   : out Compiled_Code;
                      Program  : String;
                      Base_Dir : String := "")
   is
      Tokens : Scanning.Token_List :=
                 Scanning.Tokenize (Template => Program,
                                    Base_Dir => (if Base_Dir /= "" then
                                                    Base_Dir
                                                 else
                                                    Directories.Current_Directory));
   begin
      Target.Code := Parsing.Parse_Statement_Sequence (Tokens);
   end Compile;

   ---------
   -- Run --
   ---------

   procedure Run
     (Program      : Compiled_Code;
      Symbol_Table : Symbols.Table;
      Consumer     : Consumers.Consumer_Access)
   is
   begin
      Code_Trees.Interpreter.Run (Program      => Program.Code,
                                  Symbol_Table => Symbol_Table,
                                  Consumer     => Consumer);
   end Run;

   ---------
   -- Run --
   ---------

   procedure Run
     (Program      : String;
      Symbol_Table : Symbols.Table;
      Consumer     : Consumers.Consumer_Access)
   is
   begin
      Run (Program      => Compile (Program),
           Symbol_Table => Symbol_Table,
           Consumer     => Consumer);
   end Run;

   ---------------------
   -- Expand_Template --
   ---------------------

   procedure Expand_Template
     (Template        : String;
      Symbol_Table    : Symbols.Table;
      Target_Filenane : String)
   is
      use Consumers.File_Writer;

      Cons : constant Consumers.Consumer_Access := Open (Target_Filenane);
   begin
      Run (Program      => Template,
           Symbol_Table => Symbol_Table,
           Consumer     => Cons);
   end Expand_Template;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Compiled_Code) is
   begin
      --        Put_Line ("Finalizing compiled code");
      --        pragma Compile_Time_Warning (True, "Compiled code finalization disabled");
      Code_Trees.Delete (Object.Code);

      --        Put_Line ("Done");
   end Finalize;

end Protypo.API.Interpreter;
