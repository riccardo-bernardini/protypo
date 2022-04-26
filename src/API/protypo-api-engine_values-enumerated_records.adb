pragma Ada_2012;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

package body Protypo.Api.Engine_Values.Enumerated_Records is
   -----------
   -- To_Id --
   -----------

   function To_Id (X : Field_Name) return ID
   is
      use Ada.Characters.Handling;
      use Ada.Strings.Fixed;

      F : constant ID := Id(To_Lower (X'Image));
   begin
      if Prefix = "" then
         return F;
      end if;

      declare
         P : constant String := To_Lower (Prefix);
      begin
         if F'Length > P'Length and then Head (String(F), P'Length) = P then
            return ID (Tail (String (F), F'First - P'Length));

         else
            raise Program_Error
              with "Enumerative value '" & String (F) & "' "
              & "has not prefix '" & String (P) & "'";
         end if;
      end;
   end To_Id;

   --------------
   -- To_Array --
   --------------

   function To_Array (Db : Multi_Aggregate) return Engine_Value_Array
   is
      use Handlers;

      Result : Engine_Value_Array;
   begin
      for Element of Db loop
         Result.Append (Create (Record_Interface_Access (Make_Record (Element))));
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

   function Make_Value (Init : Aggregate_Type) return Record_Value
   is (Handlers.Create (Handlers.Record_Interface_Access (Make_Record (Init))));

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Item   : in out Enumerated_Record;
      Values : Aggregate_Type)
   is
      use Engine_Value_Holders;
   begin
      for Field in Values'Range loop
         Item.Set (Field, Element (Values (Field)));
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
      Item.Map.Include (Key      => To_ID (Field),
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
