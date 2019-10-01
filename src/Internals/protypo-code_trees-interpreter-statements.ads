private
package Protypo.Code_Trees.Interpreter.Statements is



   procedure Run (Status  : Interpreter_Access;
                  Program : not null Node_Access)
     with
       Pre => Program.Class in Statement_Classes;

   procedure Run (Status  : Interpreter_Access;
                  Program : Node_Vectors.Vector);


end Protypo.Code_Trees.Interpreter.Statements;
