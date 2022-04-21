with Protypo.Api.Engine_Values;
with Protypo.Api.Symbols;
with Protypo.Api.Consumers;

with Ada.Containers.Doubly_Linked_Lists;

package Protypo.Code_Trees.Interpreter is
   procedure Run (Program      : Parsed_Code;
                  Symbol_Table : Api.Symbols.Table;
                  Consumer     : Api.Consumers.Consumer_Access);

   Bad_Iterator : exception;
   Bad_Field    : exception;

private
   type Symbol_Table_Access is not null access Api.Symbols.Table;


   type Break_Type is (Exit_Statement, Return_Statement, None);

   type Break_Status (Breaking_Reason : Break_Type := None) is
      record
         case Breaking_Reason is
            when None =>
               null;

            when Exit_Statement =>
               Loop_Label : Label_Type;

            when Return_Statement =>
               Result : Api.Engine_Values.Engine_Value_Array;
         end case;
      end record;

   No_Break : constant Break_Status := (Breaking_Reason => None);

   use type Api.Consumers.Consumer_Access;

   package Consumer_Stacks is
     new Ada.Containers.Doubly_Linked_Lists (Api.Consumers.Consumer_Access);

   subtype Consumer_Stack is Consumer_Stacks.List;

   type Interpreter_Type is tagged limited
      record
         Break                          : Break_Status;
         Symbol_Table                   : Api.Symbols.Table;
         Saved_Consumers                : Consumer_Stack;
         Consumer_Without_Escape_Cursor : Api.Symbols.Protypo_Tables.Cursor;
         Consumer_With_Escape_Cursor    : Api.Symbols.Protypo_Tables.Cursor;
      end record;


   type Interpreter_Access is not null access Interpreter_Type;


   procedure Push_Consumer (Interpreter : Interpreter_Access;
                            Consumer    : Api.Consumers.Consumer_Access);

   procedure Pop_Consumer (Interpreter :  Interpreter_Access);

   function Do_Escape (Status : Interpreter_Access;
                       Input  : String)
                       return String;
   --  Apply the #...# escape expression inside Input.

end Protypo.Code_Trees.Interpreter;
