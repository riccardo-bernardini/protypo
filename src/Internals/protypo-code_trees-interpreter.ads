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

   type Interpreter_Status is limited
      record
         Break        : Break_Status;
         Symbol_Table : Api.Symbols.Table;
      end record;

   type Status_Access is not null access Interpreter_Status;

   --
   --     type Interpreter_Result (Reason : Termination_Reason := End_Of_Code) is
   --        record
   --           case Reason is
   --              when End_Of_Code =>
   --                 null;
   --
   --              when Return_Statement =>
   --                 Result : Engine_Value_Vectors.Vector;
   --
   --              when Exit_Statement =>
   --                 Label  : Label_Type;
   --
   --              when Expression =>
   --                 Value : Engine_Value;
   --           end case;
   --        end record;

   --     No_Result : constant Interpreter_Result := (Reason => End_Of_Code);


   procedure Run (Program : not null Node_Access;
                  Status  : Status_Access)
     with
       Pre => Program.Class in Statement_Classes;

   procedure Run (Program : Node_Vectors.Vector;
                  Status  : Status_Access);

   function Eval (Expr   : not null Node_Access;
                  Status : Status_Access)
                  return Engine_Value_Vectors.Vector
     with
       Pre => Expr.Class in Code_Trees.Expression,
       Post => not Eval'Result.Is_Empty;

end Protypo.Code_Trees.Interpreter;
