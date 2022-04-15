with Protypo.Api.Engine_Values.Engine_Value_Vectors;
use Protypo.Api.Engine_Values;

package Protypo.Api.Callback_Utilities is

   type Class_Array is array (Positive range <>) of Engine_Value_Class;

   function Match_Signature (Parameters : Engine_Value_Vectors.Vector;
                             Signature  : Class_Array)
                             return Boolean;

   function Is_A (Parameters : Engine_Value_Vectors.Vector;
                  Index      : Positive;
                  Class      : Engine_Value_Class)
                  return Boolean;

   function Get (Parameters : Engine_Value_Vectors.Vector;
                 Index      : Positive)
                 return Engine_Value;

   function Get_Parameter (Parameters : Engine_Value_Vectors.Vector;
                           Index      : Positive)
                           return String
     with
       Pre => Is_A (Parameters => Parameters,
                    Index      => Index,
                    Class      => Text);

end Protypo.Api.Callback_Utilities;
