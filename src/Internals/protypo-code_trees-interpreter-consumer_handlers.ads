with Protypo.Api.Engine_Values.handlers;
with Protypo.Api.Engine_Values.Engine_Value_Vectors;
with Protypo.Api.Engine_Values.Parameter_Lists;

private package Protypo.Code_Trees.Interpreter.Consumer_Handlers is
   type Consumer_Callback is
     new Api.Engine_Values.handlers.Function_Interface
   with
     private;

   type Consumer_Callback_Access is access all Consumer_Callback;

   overriding function Process (Fun       : Consumer_Callback;
                                Parameter : Engine_Value_vectors.Vector)
                                return Engine_Value_Vectors.Vector;

   overriding function Signature (Fun : Consumer_Callback)
                                  return Parameter_Lists.Parameter_Signature;

   function Create (Consumer    : Api.Consumers.Consumer_Access;
                    With_Escape : Boolean;
                    Status      : Interpreter_Access)
                    return Api.Engine_Values.handlers.Function_Interface_Access;

   function User_Consumer (Item : Consumer_Callback)
                           return Api.Consumers.Consumer_Access;
private
   type Consumer_Callback is
     new Api.Engine_Values.handlers.Function_Interface
   with
      record
         Consumer    : Api.Consumers.Consumer_Access;
         With_Escape : Boolean;
         Status      : Interpreter_Access;
      end record;


   function Create (Consumer    : Api.Consumers.Consumer_Access;
                    With_Escape : Boolean;
                    Status      : Interpreter_Access)
                    return Api.Engine_Values.handlers.Function_Interface_Access
   is (new Consumer_Callback'(Consumer => Consumer,
                              With_Escape =>  With_Escape,
                              Status      => Status));

   function User_Consumer (Item : Consumer_Callback)
                           return Api.Consumers.Consumer_Access
   is (Item.Consumer);

end Protypo.Code_Trees.Interpreter.Consumer_Handlers;
