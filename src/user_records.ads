with Protypo.Api.Engine_Values.Enumerated_Records;

package User_Records is
   type User_Field is (First_Name, Last_Name, Telephone);

   package User_Record_Package is
     new Protypo.Api.Engine_Values.Enumerated_Records (User_Field);
end User_Records;
