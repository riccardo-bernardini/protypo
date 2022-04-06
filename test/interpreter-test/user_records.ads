with Protypo.Api.Engine_Values.Enumerated_Records;
with Protypo.Api.Engine_Values.Engine_Value_Vectors;


package User_Records is
   type User_Field is (First_Name, Last_Name, Telephone);

   package User_Record_Package is
     new Protypo.Api.Engine_Values.Enumerated_Records (User_Field);

   function Split_Bit (Params : Protypo.Api.Engine_Values.Engine_Value_Vectors.Vector)
                       return Protypo.Api.Engine_Values.Engine_Value_Vectors.Vector;
end User_Records;
