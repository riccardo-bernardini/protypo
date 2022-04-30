with Protypo.Api.Engine_Values.Handlers;
with Protypo.Api.Engine_Values.Parameter_Lists;

private package Protypo.Code_Trees.Interpreter.Consumer_Handlers is
   use Protypo.Api.Engine_Values;

   type Consumer_Callback is
     new Handlers.Function_Interface
   with
     private;

   type Consumer_Callback_Access is access all Consumer_Callback;

   overriding function Process (Fun       : Consumer_Callback;
                                Parameter : Engine_Value_Array)
                                return Engine_Value_Array;

   overriding function Signature (Fun : Consumer_Callback)
                                  return Parameter_Lists.Parameter_Signature;

   function New_Consumer (Consumer    : Api.Consumers.Consumer_Access;
                          With_Escape : Boolean;
                          End_Of_Line : Unbounded_String;
                          Status      : Interpreter_Access)
                          return Api.Engine_Values.Handlers.Procedure_Interface'Class;

   function User_Consumer (Item : Consumer_Callback)
                           return Api.Consumers.Consumer_Access;
private
   type Consumer_Callback is
     new Api.Engine_Values.Handlers.Function_Interface
   with
      record
         Consumer      : Api.Consumers.Consumer_Access;
         With_Escape   : Boolean;
         End_Of_Line   : Unbounded_String;
         Status        : Interpreter_Access;
      end record;


   function Create (Consumer    : Api.Consumers.Consumer_Access;
                    With_Escape : Boolean;
                    End_Of_Line : Unbounded_String;
                    Status      : Interpreter_Access)
                    return Api.Engine_Values.Handlers.Function_Interface_Access
   is (new Consumer_Callback'(Consumer => Consumer,
                              With_Escape =>  With_Escape,
                              End_Of_Line => End_Of_Line,
                              Status      => Status));

   function User_Consumer (Item : Consumer_Callback)
                           return Api.Consumers.Consumer_Access
   is (Item.Consumer);

end Protypo.Code_Trees.Interpreter.Consumer_Handlers;
