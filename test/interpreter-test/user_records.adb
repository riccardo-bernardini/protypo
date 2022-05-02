pragma Ada_2012;
with Protypo.Api.Engine_Values.Parameter_Lists; use Protypo.Api.Engine_Values.Parameter_Lists;
package body User_Records is

   ---------------
   -- Split_Bit --
   ---------------

   function Split_Bit
     (Params : Protypo.Api.Engine_Values.Engine_Value_Array)
      return Protypo.Api.Engine_Values.Engine_Value_Array
   is
      use Protypo.Api.Engine_Values;

      Parameters : Parameter_List := Create (Params);

      X : constant Integer := Get_Integer (Shift (Parameters));
      Y : constant Integer := Get_Integer (Shift (Parameters), 2);

      Result : Engine_Value_Array;

   begin
      Result (1) := Create (X / Y);
      Result (2) := Create (X mod Y);
      return Result;
   end Split_Bit;

end User_Records;
