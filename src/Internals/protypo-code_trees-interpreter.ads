with Protypo.Api.Symbols;
with Protypo.Api.Consumers;
with Protypo.Api.Engine_Values;

package Protypo.Code_Trees.Interpreter is
   use Protypo.Api.Engine_Values;

   procedure Run (Program      : Parsed_Code;
                  Symbol_Table : Api.Symbols.Table;
                  Consumer     : Api.Consumers.Consumer_Access);

   Bad_Iterator : exception;
   Bad_Field    : exception;
private
  package Engine_Value_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Engine_Value);

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
               Result : Engine_Value_Vectors.Vector;
         end case;
      end record;

   No_Break : constant Break_Status := (Breaking_Reason => None);

   type Interpreter_Type is tagged limited
      record
         Break        : Break_Status;
         Symbol_Table : Api.Symbols.Table;
      end record;

   type Interpreter_Access is not null access Interpreter_Type;





end Protypo.Code_Trees.Interpreter;
