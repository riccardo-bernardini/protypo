with Protypo.Api.Engine_Values.Engine_Value_Vectors;

package Callbacks is
   use Protypo.Api;

   function Sin (X : Engine_Values.Engine_Value_Vectors.Vector)
              return Engine_Values.Engine_Value_Vectors.Vector;
end Callbacks;
