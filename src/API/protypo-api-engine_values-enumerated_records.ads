with Ada.Containers.Indefinite_Ordered_Maps;
with Protypo.Api.Engine_Values.Constant_Wrappers;
--
-- This package provides resources to define a record-like variable
-- where the field names are specified as values of an enumerative
-- type.  Since the enumerative type is not specified a priori, we
-- need to make this package generic.
--
-- The record-like variable will appear to have fields whose name is
-- obtained by removing the Prefix from the enumerative values.  This
-- is done in order to turn around the fact that some words are reserved
-- in Ada.
--
-- > For example, suppose one wants to export the field "range."
-- Unfortunately, it is not possible to have "range" as an enumerative value.
-- One can solve this problem by using --- for example --- `Prefix="Field_"`
-- so that the enumerative value `Field_Range` can be used.
--
generic
   type Field_Name is (<>);

   Prefix : String := "";
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
   -- Create a new record-like handler initialized with the values
   -- specified in the aggregate

   function Make_Value (Init : Aggregate_Type) return Engine_Value;
   -- Syntactic sugar that creates the handler and embed it into
   -- an Engine_Value. Equivalent to
   --
   --      Create (Record_Interface_Access (Make_Record (Init)))


   procedure Fill (Item   : in out Enumerated_Record;
                   Values : Aggregate_Type);
   -- Write the values to the field of the Enumerated_Record

   procedure Set (Item  : in out Enumerated_Record;
                  Field : Field_Name;
                  Value : Engine_Value);
   -- Write a field of the Enumerated_Record

   overriding
   function Get (Item  : Enumerated_Record;
                 Field : ID)
                 return Handler_Value;

   overriding
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

   function Apply_Prefix (X : Id) return ID
   is (ID (Prefix & String (X)));

   function Get (Item : Enumerated_Record; Field : Id) return Handler_Value
   is (if Item.Map.Contains (Apply_Prefix (Field)) then
          Constant_Wrappers.To_Handler_Value (Item.Map (Apply_Prefix (Field)))
       else
          raise Unknown_Field);

end Protypo.Api.Engine_Values.Enumerated_Records;
