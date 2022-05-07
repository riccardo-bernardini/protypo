with Protypo.Api.Engine_Values;

package Multi_Test.Test_Results is
   procedure New_Suite (Label : String);

   procedure Success (Params : Protypo.Api.Engine_Values.Engine_Value_Array);

   procedure Failure (Params : Protypo.Api.Engine_Values.Engine_Value_Array);

   procedure Print_Report;

end Multi_Test.Test_Results;
