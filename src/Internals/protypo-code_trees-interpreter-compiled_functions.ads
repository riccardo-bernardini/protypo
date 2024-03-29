with Protypo.Api.Engine_Values.Handlers;
with Protypo.Api.Engine_Values.Parameter_Lists;
private package Protypo.Code_Trees.Interpreter.Compiled_Functions is
   --     type Compiled_Function is
   --       new Api.Engine_Values.Function_Interface
   --     with
   --       private;

   type Compiled_Function is
     new Api.Engine_Values.handlers.Function_Interface
   with
      record
         Function_Body : Node_Vectors.Vector;
         Parameters    : Parameter_Specs;
         Status        : Interpreter_Access;
      end record;

   overriding function Process (Fun       : Compiled_Function;
                                Parameter : Engine_Value_vectors.Vector)
                                return Engine_Value_vectors.Vector;

   overriding function Signature (Fun : Compiled_Function)
                                  return Api.Engine_Values.Parameter_Lists.Parameter_Signature;

   --     function Create (Function_Body : Node_Vectors.Vector) return Integer;
end Protypo.Code_Trees.Interpreter.Compiled_Functions;
