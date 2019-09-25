with Protypo.Api.Symbols;
with Protypo.Api.Consumers;
with Protypo.Api.Engine_Values;

package Protypo.Code_Trees.Interpreter is
   use Protypo.Api.Engine_Values;

   procedure Run (Program      : Parsed_Code;
                  Symbol_Table : Api.Symbols.Table;
                  Consumer     : Api.Consumers.Consumer_Access);

private

   type Symbol_Table_Access is not null access Api.Symbols.Table;

   package Engine_Value_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Engine_Value);


   function To_Vector (X : Engine_Value_Array)
                       return Engine_Value_Vectors.Vector;

   function To_Array (X : Engine_Value_Vectors.Vector)
                      return Engine_Value_Array;

   type Termination_Reason is
     (
      End_Of_Code,
      Return_Statement,
      Exit_Statement,
      Expression
     );

   type Interpreter_Result (Reason : Termination_Reason := End_Of_Code) is
      record
         case Reason is
            when End_Of_Code =>
               null;

            when Return_Statement =>
               Result : Engine_Value_Vectors.Vector;

            when Exit_Statement =>
               Label  : Label_Type;

            when Expression =>
               Value : Engine_Value;
         end case;
      end record;

   No_Result : constant Interpreter_Result := (Reason => End_Of_Code);


    function Run
     (Program      : not null Node_Access;
      Symbol_Table : Symbol_Table_Access)
      return Interpreter_Result;

   function Run
     (Program      : Node_Vectors.Vector;
      Symbol_Table : Symbol_Table_Access)
      return Interpreter_Result;
end Protypo.Code_Trees.Interpreter;
