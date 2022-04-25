with Protypo.Api.Engine_Values.Handlers;
with Protypo.Api.Engine_Values.Parameter_Lists;
private package Protypo.Code_Trees.Interpreter.Compiled_Procedures is
   use Protypo.Api.Engine_Values;

   type Compiled_Procedure is
     new Api.Engine_Values.Handlers.Procedure_Interface
   with
      record
         Procedure_Body : Node_Vectors.Vector;
         Parameters     : Parameter_Specs;
         Status         : Interpreter_Access;
      end record;

   overriding procedure Process (Fun       : Compiled_Procedure;
                                 Parameter : Engine_Value_Array);

   overriding function Signature (Fun : Compiled_Procedure)
                                   return Api.Engine_Values.Parameter_Lists.Parameter_Signature;
end Protypo.Code_Trees.Interpreter.Compiled_Procedures;
