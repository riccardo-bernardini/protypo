pragma Ada_2012;
package body Protypo.Api.Engine_Values.Enumerated_Records is

   -----------------
   -- Make_Record --
   -----------------

   function Make_Record
     (Init : Aggregate_Type := Void_Aggregate)
      return Enumerated_Record_Access
   is
      Result : constant Enumerated_Record_Access :=
                 new Enumerated_Record'(Map => Record_Maps.Empty_Map);
   begin
      Result.Fill (Init);
      return Result;
   end Make_Record;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Item   : in out Enumerated_Record;
      Values : Aggregate_Type)
   is
   begin
      for Field in Values'Range loop
         Item.Set (Field, Values (Field));
      end loop;
   end Fill;

   ---------
   -- Set --
   ---------

   procedure Set
     (Item  : in out Enumerated_Record;
      Field : Field_Name;
      Value : Engine_Value)
   is
   begin
      Item.Map.Include (Key      => Field'Image,
                        New_Item => Value);
   end Set;

end Protypo.Api.Engine_Values.Enumerated_Records;
