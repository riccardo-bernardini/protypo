with Protypo.Api.Engine_Values;
private package Protypo.Code_Trees.Interpreter.Consumer_Handlers is
   type Consumer_Callback is
     new Api.Engine_Values.Function_Interface
   with
     private;

   overriding function Process (Fun       : Consumer_Callback;
                                Parameter : Engine_Value_Array)
                                return Engine_Value_Array;

   overriding function Default_Parameters (Fun : Consumer_Callback)
                                           return Engine_Value_Array;

   function Create (Consumer    : Api.Consumers.Consumer_Access;
                    With_Escape : Boolean;
                    Status      : Interpreter_Access)
                    return Api.Engine_Values.Function_Interface_Access;
private
   type Consumer_Callback is
     new Api.Engine_Values.Function_Interface
   with
      record
         Consumer    : Api.Consumers.Consumer_Access;
         With_Escape : Boolean;
         Status      : Interpreter_Access;
      end record;


   function Create (Consumer    : Api.Consumers.Consumer_Access;
                    With_Escape : Boolean;
                    Status      : Interpreter_Access)
                    return Api.Engine_Values.Function_Interface_Access
   is (new Consumer_Callback'(Consumer => Consumer,
                              With_Escape =>  With_Escape,
                              Status      => Status));
end Protypo.Code_Trees.Interpreter.Consumer_Handlers;
