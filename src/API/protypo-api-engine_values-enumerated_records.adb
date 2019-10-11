pragma Ada_2012;
--------------------------------------------------
-- Protypo.Api.Engine_Values.Enumerated_Records --
--------------------------------------------------

package body Protypo.Api.Engine_Values.Enumerated_Records is
   --------------
   -- To_Array --
   --------------

   function To_Array (Db : Multi_Aggregate) return Engine_Value_Array
   is
      Result : Engine_Value_Array (Db'Range);
   begin
      for K in Db'Range loop
         Result (K) := Create (Record_Interface_Access (Make_Record (Db (K))));
      end loop;

      return Result;
   end To_Array;

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

   ----------------
   -- Make_Value --
   ----------------

   function Make_Value (Init : Aggregate_Type) return Engine_Value
   is (Create (Record_Interface_Access (Make_Record (Init))));

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


   --------------
   -- Is_Field --
   --------------

   function Is_Field (Item : Enumerated_Record; Field : ID) return Boolean
   is
   begin
      return Item.Map.Contains (Field);
   end Is_Field;


end Protypo.Api.Engine_Values.Enumerated_Records;
