with Protypo.Api.Engine_Values.Array_Wrappers;

package Integer_Arrays is
   type Int_Array is array (Positive range <>) of Integer;

   package Wrappers is
     new Protypo.Api.Engine_Values.Array_Wrappers
       (Element_Type => Integer,
        Array_Type   => Int_Array,
        Create       => Protypo.Api.Engine_Values.Create);
end Integer_Arrays;
