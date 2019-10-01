--
-- A name is, more or less, a variable that can include both indexes
-- array-like and fields record-like.  For example,
--
--   foo.bar(5).gim
--
-- The evaluation of a name does not return a scalar, but a reference.
-- Why? Because a name can be on the left hand side of an assignment
-- like
--
--   foo.bar(5) := Pi;
--
-- Therefore, when we evaluate a name we expect to end with some kind
-- of "pointer" (a reference) that we can use to read a value (if the
-- name is in an expression) or write it (if it is on the LHS).
--
-- We have several types of references, see Value_Name_Class.
--

private package Protypo.Code_Trees.Interpreter.Names is

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

   function Eval_Name (Status : Interpreter_Access;
                       Expr   : not null Node_Access)
                       return Name_Reference
     with
       Pre => Expr.Class in Name;

end Protypo.Code_Trees.Interpreter.Names;
