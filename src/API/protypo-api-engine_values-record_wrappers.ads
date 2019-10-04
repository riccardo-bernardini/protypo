with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Less_Case_Insensitive;

--
-- This package provides a wrapper around to a
--
package Protypo.Api.Engine_Values.Record_Wrappers is
   package Record_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => Engine_Value,
        "<"          => Ada.Strings.Less_Case_Insensitive);

   subtype Record_Map is Record_Maps.Map;

   type Record_Map_Reference (Ref : access Record_Map) is limited private
     with Implicit_Dereference => Ref;

   type Record_Wrapper is new Record_Interface with private;
   type Record_Wrapper_Access is access Record_Wrapper;

   function Get (X     : Record_Wrapper;
                 Field : String)
                 return Engine_Value;

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


end Protypo.Api.Engine_Values.Record_Wrappers;
