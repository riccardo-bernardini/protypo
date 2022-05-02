with Protypo.Api.Engine_Values.Enumerated_Records;


package User_Records is
   type User_Field is (First_Name, Last_Name, Telephone);

   package User_Record_Package is
     new Protypo.Api.Engine_Values.Enumerated_Records (User_Field);

   function Split_Bit (Params : Protypo.Api.Engine_Values.Engine_Value_Array)
                       return Protypo.Api.Engine_Values.Engine_Value_Array;
end User_Records;
