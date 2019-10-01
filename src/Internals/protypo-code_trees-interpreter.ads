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


   procedure Run (Status  : Interpreter_Access;
                  Program : not null Node_Access)
     with
       Pre => Program.Class in Statement_Classes;

   procedure Run (Status  : Interpreter_Access;
                  Program : Node_Vectors.Vector);

   function Eval_Expression (Status : Interpreter_Access;
                             Expr   : not null Node_Access)
                             return Engine_Value_Vectors.Vector
     with
       Pre => Expr.Class in Code_Trees.Expression,
       Post => not Eval_Expression'Result.Is_Empty;

   function Eval_Scalar (Status : Interpreter_Access;
                         Expr   : not null Node_Access)
                         return Engine_Value;


   function Eval_Vector (Status : Interpreter_Access;
                         Expr   : Node_Vectors.Vector)
                         return Engine_Value_Vectors.Vector;


   type Value_Name_Class is
     (
      Array_Reference,
      Record_Reference,
      Variable_Reference,
      Constant_Reference,
      Function_Reference,
      Function_Call
     );

   subtype Function_Classes is
     Value_Name_Class range Function_Reference .. Function_Call;

   type Name_Reference (Class : Value_Name_Class := Constant_Reference) is
      record
         case Class is
         when Array_Reference =>
            Array_Handler : Array_Interface_Access;

         when Record_Reference =>
            Record_Handler : Record_Interface_Access;

         when Variable_Reference =>
            Variable_Handler : Reference_Interface_Access;

         when Constant_Reference =>
            Costant_Handler : Constant_Interface_Access;

         when Function_Reference | Function_Call =>
            Function_Handler : Function_Interface_Access;
            Parameters       : Engine_Value_Vectors.Vector;

         end case;
      end record
     with
       Dynamic_Predicate =>
         (if Name_Reference.Class = Function_Reference
            then
              Name_Reference.Parameters.Is_Empty);


   subtype Evaluable_Classes is Value_Name_Class
     with
       Static_Predicate =>
         Evaluable_Classes in Function_Call | Constant_Reference | Variable_Reference;

   subtype Function_Call_Reference is Name_Reference (Function_Call);

   function Eval_Name (Status : Interpreter_Access;
                       Expr   : not null Node_Access)
                       return Name_Reference
     with
       Pre => Expr.Class in Name;


   function Call_Function (Reference : Function_Call_Reference)
                           return Engine_Value_Array;

   function To_Value (Ref : Name_Reference) return Engine_Value_Array
     with Pre => Ref.Class in Evaluable_Classes;

end Protypo.Code_Trees.Interpreter;
