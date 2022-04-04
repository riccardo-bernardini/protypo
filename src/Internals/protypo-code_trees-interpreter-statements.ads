private
package Protypo.Code_Trees.Interpreter.Statements is



   procedure Run (Status  : Interpreter_Access;
                  Program : not null Node_Access)
     with
       Pre => Program.Class in Statement_Classes;

   procedure Run (Status  : Interpreter_Access;
                  Program : Node_Vectors.Vector);

   procedure Do_Procedure_Call (Status : Interpreter_Access;
                                Name   : Unbounded_id;
                                Params : Node_Vectors.Vector);
   -- Why do we export this?  Because the "capture" in the expression
   -- evaluation package needs to call a procedure



end Protypo.Code_Trees.Interpreter.Statements;
