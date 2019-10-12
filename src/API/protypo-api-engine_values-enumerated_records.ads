with Ada.Containers.Indefinite_Ordered_Maps;
with Protypo.Api.Engine_Values.Constant_Wrappers;

generic
   type Field_Name is (<>);
package Protypo.Api.Engine_Values.Enumerated_Records is
   type Aggregate_Type is array (Field_Name) of Engine_Value;
   -- Type that allows to specify an enumerated record similarly
   -- to an Ada aggregate, for example,
   --
   --     (First_Name => Create ("Pippo"),
   --      Last_Name  => Create ("Recupero"),
   --      Telephone  => Create ("3204365972"))

   Void_Aggregate : constant Aggregate_Type := (others => Void_Value);

   type Multi_Aggregate is array (Positive range <>) of Aggregate_Type;
   -- Array of aggregate type.  It allows to write constant "databases"
   -- of enumerated records

   function To_Array (Db : Multi_Aggregate) return Engine_Value_Array
     with Post => (for all Item of To_Array'Result => Item.Class = Record_Handler);
   -- Convert an array of aggregates into an Engine_Value_Array whose
   -- entries are Enumerated_Records.
   -- Very useful in initializing other wrappers (e.g., from Array_Wrappers)


   type Enumerated_Record is new Record_Interface with private;
   type Enumerated_Record_Access is access Enumerated_Record;


   function Make_Record (Init : Aggregate_Type := Void_Aggregate)
                         return Enumerated_Record_Access;

   function Make_Value (Init : Aggregate_Type) return Engine_Value;

   procedure Fill (Item   : in out Enumerated_Record;
                   Values : Aggregate_Type);

   procedure Set (Item  : in out Enumerated_Record;
                  Field : Field_Name;
                  Value : Engine_Value);

   function Get (Item  : Enumerated_Record;
                 Field : ID)
                 return Handler_Value;

   function Is_Field (Item : Enumerated_Record; Field : ID) return Boolean;
private
   package Record_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => ID,
        Element_Type => Engine_Value);


   type Enumerated_Record is
     new Record_Interface
   with
      record
         Map : Record_Maps.Map;
      end record;

   function Get (Item : Enumerated_Record; Field : Id) return Handler_Value
   is (if Item.Map.Contains (Field) then
          Constant_Wrappers.To_Handler_Value (Item.Map (Field))
       else
          raise Unknown_Field);

end Protypo.Api.Engine_Values.Enumerated_Records;
