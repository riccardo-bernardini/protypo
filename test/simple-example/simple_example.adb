with Protypo.Api.Interpreters;
with Protypo.Api.Consumers.File_Writer;
with Protypo.Api.Engine_Values.Handlers;

with Callbacks;

procedure Simple_Example is
   use Protypo.Api;
   use Protypo.Api.Consumers;

   Engine   : Interpreters.Interpreter_Type;

   Program  : constant Interpreters.Template_Type :=
                "#{  [sin(1.5)=#sin(1.5)#, 42=#the_answer#] }#";

   Consumer : constant Consumer_Access :=
                File_Writer.Open (File_Writer.Standard_Error_Special_Name);

   Z        : constant Interpreters.Compiled_Code := Interpreters.Compile (Program);

begin
   Interpreters.Dump (Z);

   Engine.Define (Name  => "the_answer",
                  Value => Engine_Values.Create (42));

   Engine.Define (Name  => "sin",
                  Value => Engine_Values.Handlers.Create (Callbacks.Sin'Access));

   Engine.Run (Program, Consumer);
end Simple_Example;
