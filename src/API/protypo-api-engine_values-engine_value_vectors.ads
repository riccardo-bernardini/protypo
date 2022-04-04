with Ada.Containers.Indefinite_Vectors;

package Protypo.Api.Engine_Values.Engine_Value_Vectors is
  new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                         Element_Type => Engine_Value);


--  package Engine_Value_Vectors is
--     subtype Engine_Value_Array is Engine_Value_Vectors.Vector;
--
--     No_Value   : constant Engine_Value_Array := Engine_Value_Vectors.Empty_Vector;
--
--  end Protypo.Api.Engine_Values.Value_Arrays;
