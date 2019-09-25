with Protypo.Api.Engine_Values;
package Protypo.Code_Trees.Interpreter.Consumer_Handlers is
   type Consumer_Callback is
     new Api.Engine_Values.Function_Interface
   with
     private;

   overriding function Process (Fun       : Consumer_Callback;
                                Parameter : Engine_Value_Array)
                                return Engine_Value_Array;

   function Create (Consumer : Api.Consumers.Consumer_Access)
                    return Api.Engine_Values.Function_Interface_Access;
private
   type Consumer_Callback is
     new Api.Engine_Values.Function_Interface
   with
      record
         Consumer : Api.Consumers.Consumer_Access;
      end record;


   function Create (Consumer : Api.Consumers.Consumer_Access)
                 return Api.Engine_Values.Function_Interface_Access
   is (new Consumer_Callback'(Consumer => Consumer));
end Protypo.Code_Trees.Interpreter.Consumer_Handlers;
