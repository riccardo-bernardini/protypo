with Protypo.Code_Trees.Interpreter.Names;

private
package Protypo.Code_Trees.Interpreter.Expressions is



   function To_Vector (X : Engine_Value_Array)
                       return Engine_Value_Vectors.Vector;

   function To_Array (X : Engine_Value_Vectors.Vector)
                      return Engine_Value_Array;


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

   function Eval_Iterator (Status : Interpreter_Access;
                           Expr   : not null Node_Access)
                            return Iterator_Interface_Access;


   subtype Function_Call_Reference is Names.Name_Reference (Names.Function_Call);

   function Call_Function (Reference : Function_Call_Reference)
                           return Engine_Value_Array;


   subtype Evaluable_Classes is Names.Value_Name_Class
     with
       Static_Predicate =>
         Evaluable_Classes in
           Names.Function_Call
             | Names.Constant_Reference
               | Names.Variable_Reference;

   function To_Value (Ref : Names.Name_Reference) return Engine_Value_Array
     with Pre => Ref.Class in Evaluable_Classes;
   -- Access a reference and return the value "pointed at."  Note that only
   -- a subset of the possible "name values" have a scalar value associated


end Protypo.Code_Trees.Interpreter.Expressions;
