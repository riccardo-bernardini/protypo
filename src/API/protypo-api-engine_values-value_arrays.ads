with ada.Containers.Indefinite_Vectors;

package Protypo.Api.Engine_Values.Value_Arrays is
   package Engine_Value_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => positive,
                                            Element_Type => Engine_Value);

   subtype Engine_Value_Array is Engine_Value_Vectors.Vector;

   No_Value   : constant Engine_Value_Array := Engine_Value_Vectors.Empty_Vector;

end Protypo.Api.Engine_Values.Value_Arrays;
