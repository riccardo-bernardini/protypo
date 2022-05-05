with Protypo.Api.Engine_Values.Array_Wrappers;

package Integer_Arrays is
   use Protypo.Api.Engine_Values;

   type Int_Array is array (Positive range <>) of Integer;

   --  function Image (Item   : Integer;
   --                  Format : String) return String;

   package Wrappers is
     new Protypo.Api.Engine_Values.Array_Wrappers
       (Element_Type => Integer,
        Index_Type   => Positive,
        Array_Type   => Int_Array,
        Get_Value    => Protypo.Api.Engine_Values.Get_Integer,
        Image        => Protypo.Api.Engine_Values.Image,
        Name         => "integer array",
        Create       => Protypo.Api.Engine_Values.Create);
end Integer_Arrays;
