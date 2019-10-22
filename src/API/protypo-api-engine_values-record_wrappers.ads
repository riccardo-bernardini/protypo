with Ada.Containers.Indefinite_Ordered_Maps;

--
-- This package provides a wrapper around a map structure. The field names
-- are the keys of the map.
--
package Protypo.Api.Engine_Values.Record_Wrappers is
   package Record_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => ID,
        Element_Type => Engine_Value);

   subtype Record_Map is Record_Maps.Map;
   type Record_Map_Reference (Ref : access Record_Map) is limited private
     with Implicit_Dereference => Ref;




   type Record_Wrapper is new Record_Interface with private;
   type Record_Wrapper_Access is access Record_Wrapper;

   overriding function Get (X : Record_Wrapper; Field : ID) return Handler_Value;
   overriding function Is_Field (X : Record_Wrapper; Field : ID) return Boolean;

   function Create_Wrapper return Record_Wrapper_Access;

   function Map (Item : in out Record_Wrapper) return Record_Map_Reference;
private

   type Record_Map_Reference (Ref : access Record_Map) is limited null record;

   type Record_Wrapper is new Record_Interface
   with
      record
         Map : aliased Record_Map;
      end record;


   function Create_Wrapper return Record_Wrapper_Access
   is (new Record_Wrapper'(Map => Record_Maps.Empty_Map));

   function Map (Item : in out Record_Wrapper) return Record_Map_Reference
   is (Record_Map_Reference'(Ref => Item.Map'Access));


   function Is_Field (X : Record_Wrapper; Field : ID) return Boolean
   is (X.Map.Contains (Field));

end Protypo.Api.Engine_Values.Record_Wrappers;
