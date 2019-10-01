with Ada.Finalization;

with Protypo.API.Consumers;
with Protypo.API.Symbols;

private with Protypo.Code_Trees;

package Protypo.API.Interpreter is
   type Compiled_Code is limited private;

   procedure Bye (X : in out Compiled_Code);
   function Compile (Program  : String;
                     Base_Dir : String := "") return Compiled_Code;

   procedure Run (Program      : Compiled_Code;
                  Symbol_Table : Symbols.Table;
                  Consumer     : Consumers.Consumer_Access);

   procedure Run (Program      : String;
                  Symbol_Table : Symbols.Table;
                  Consumer     : Consumers.Consumer_Access);


   procedure Expand_Template (Template        : String;
                              Symbol_Table    : Symbols.Table;
                              Target_Filenane : String);
private
   type Compiled_Code is
     new Ada.Finalization.Limited_Controlled
   with
      record
         Code : Code_Trees.Parsed_Code;
      end record;

   overriding procedure Finalize (Object : in out Compiled_Code);
end Protypo.API.Interpreter;
