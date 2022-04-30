pragma Ada_2012;
with Protypo.Code_Trees.Interpreter;
with Protypo.Scanning;
with Protypo.Parsing;

with Ada.Directories;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_Io; use Ada.Text_Io;
with Protypo.Api.Consumers.File_Writer;


package body Protypo.Api.Interpreters is
   ------------
   -- Define --
   ------------

   procedure Define (Interpreter : in out Interpreter_Type;
                     Name        : Id;
                     Value       : Symbols.Symbol_Value)
   is
   begin
      Interpreter.Symbol_Table.Create (Name          => Name,
                                       Initial_Value => Value);
   end Define;

   ------------
   -- Define --
   ------------

   procedure Define (Interpreter : in out Interpreter_Type;
                     Name        : Id;
                     Value       : Engine_Values.Engine_Value)
   is
   begin
      Interpreter.Define (Name  => Name,
                          Value => Symbols.To_Symbol_Value (Value));
   end Define;

   ------------
   -- Define --
   ------------

   procedure Define (Interpreter : in out Interpreter_Type;
                     Name        : Id;
                     Funct       : Engine_Values.Handlers.Function_Interface'Class)
   is
   begin
      Interpreter.Define (Name  => Name,
                          Value => Symbols.To_Symbol_Value (Funct));
   end Define;

   ------------
   -- Define --
   ------------

   procedure Define (Interpreter : in out Interpreter_Type;
                     Name        : Id;
                     Proc        : Engine_Values.Handlers.Procedure_Interface'Class)
   is
   begin
      Interpreter.Define (Name  => Name,
                          Value => Symbols.To_Symbol_Value (Proc));
   end Define;





   ---------
   -- Run --
   ---------

   procedure Run (Interpreter  : in out Interpreter_Type;
                  Program      : Compiled_Code;
                  Consumer     : Consumers.Consumer_Access)
   is
   begin
      Code_Trees.Interpreter.Run (Program      => Program.Code,
                                  Symbol_Table => Interpreter.Symbol_Table,
                                  Consumer     => Consumer);
   end Run;

   ---------
   -- Run --
   ---------

   procedure Run (Interpreter  : in out Interpreter_Type;
                  Program      : Template_Type;
                  Consumer     : Consumers.Consumer_Access)
   is
   begin
      Run (Program      => Compile (Program),
           Interpreter  => Interpreter,
           Consumer     => Consumer);
   end Run;

   ---------------------
   -- Expand_Template --
   ---------------------

   procedure Expand_Template (Interpreter     : in out Interpreter_Type;
                              Input_Filename  : String;
                              Target_Filenane : String)
   is
      use Consumers.File_Writer;

      Consumer : constant Consumers.Consumer_Access :=
                   Open (if Target_Filenane = "-"
                         then
                            Standard_Output_Special_Name
                         else
                            Target_Filenane);

      Template : constant Template_Type := Slurp (Input_Filename);
   begin
      Run (Interpreter => Interpreter,
           Program     => Template,
           Consumer    => Consumer);
   end Expand_Template;

   ----------
   -- Dump --
   ----------

   procedure Dump (X : Compiled_Code)
   is
   begin
      Code_Trees.Dump (X.Code);
   end Dump;

   ---------
   -- Bye --
   ---------

   procedure Bye (X : in out Compiled_Code)
   is
   begin
      Code_Trees.Delete (X.Code);
   end Bye;

   -------------
   -- Compile --
   -------------

   function Compile (Program  : Template_Type;
                     Base_Dir : String := "") return Compiled_Code is

      Tokens : Scanning.Token_List :=
                 Scanning.Tokenize (Template => Program,
                                    Base_Dir => (if Base_Dir /= "" then
                                                    Base_Dir
                                                 else
                                                    Ada.Directories.Current_Directory));
   begin
      return Compiled_Code'(Code => Parsing.Parse_Statement_Sequence (Tokens));
   end Compile;


   -------------
   -- Compile --
   -------------

   procedure Compile (Target   : out Compiled_Code;
                      Program  : Template_Type;
                      Base_Dir : String := "")
   is
      Tokens : Scanning.Token_List :=
                 Scanning.Tokenize (Template => Program,
                                    Base_Dir => (if Base_Dir /= "" then
                                                    Base_Dir
                                                 else
                                                    Ada.Directories.Current_Directory));
   begin
      Target.Code := Parsing.Parse_Statement_Sequence (Tokens);
   end Compile;

   --     --------------
   --     -- Finalize --
   --     --------------
   --
   --     overriding procedure Finalize (Object : in out Compiled_Code) is
   --     begin
   --        --        Put_Line ("Finalizing compiled code");
   --        --        pragma Compile_Time_Warning (True, "Compiled code finalization disabled");
   --        Code_Trees.Delete (Object.Code);
   --
   --        --        Put_Line ("Done");
   --     end Finalize;



end Protypo.Api.Interpreters;
