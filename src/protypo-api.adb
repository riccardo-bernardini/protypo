pragma Ada_2012;
with Protypo.Scanning;
with Protypo.Parsing;
with Protypo.Code_Trees.Interpreter;
with Protypo.Consumers.File_Writer;

package body Protypo.API is
use Ada.Finalization;
   -------------
   -- Compile --
   -------------

   function Compile (Program : String) return Compiled_Code is
      Tokens : Scanning.Token_List := Scanning.Tokenize (Program);
   begin
      return Compiled_Code'(Limited_Controlled with
                              Code => Parsing.Parse_Statement_Sequence (Tokens));
   end Compile;

   ---------
   -- Run --
   ---------

   procedure Run
     (Program      : Compiled_Code;
      Symbol_Table : Symbols.Table;
      Consumer     : in out Consumers.Consumer_Interface'Class)
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
      Consumer     : in out Consumers.Consumer_Interface'Class)
   is
   begin
      Run (Program      => Compile(Program),
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

      Cons : Writer := Open (Target_Filenane);
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
      Code_Trees.Delete (Object.Code);
   end Finalize;

end Protypo.API;
