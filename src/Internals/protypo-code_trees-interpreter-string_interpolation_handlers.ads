with Protypo.Api.Engine_Values.Handlers;
with Protypo.Api.Engine_Values.Parameter_Lists;

with Ada.Tags;

private package Protypo.Code_Trees.Interpreter.String_Interpolation_Handlers is
   use type Ada.Tags.Tag;

   type String_Interpolator is
     new Handlers.Function_Interface
       with
     private;

   function Create (Interp : Interpreter_Access)
                    return Handlers.Function_Interface_Access
     with
       Post => Create'Result'Tag = String_Interpolator'Tag;

   function Process (Fun       : String_Interpolator;
                     Parameter : Engine_Value_Vectors.Vector)
                     return Engine_Value_Vectors.Vector;

   function Signature (Fun : String_Interpolator)
                       return Parameter_Lists.Parameter_Signature;
private
   type String_Interpolator is
     new Handlers.Function_Interface
   with
      record
         Status : Interpreter_Access;
      end record;
end Protypo.Code_Trees.Interpreter.String_Interpolation_Handlers;
