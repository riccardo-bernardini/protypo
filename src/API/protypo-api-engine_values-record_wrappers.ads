with Ada.Containers.Indefinite_Ordered_Maps;

package Protypo.Api.Engine_Values.Record_Wrappers is
   package Record_Maps is
         new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                     Element_Type => Engine_Value);

   subtype Record_Map is Record_Maps.Map;

   type Record_Map_Reference(Ref: access Record_Map) is null Record
         with Implicit_Dereference => Ref;

   type Record_Wrapper is new Record_Interface with private;
   type Record_Wrapper_Access is access Record_Wrapper;

   function Get (X     : Record_Wrapper;
                 Field : String)
                 return Engine_Value;

   function Create return Record_Wrapper_Access;

   function Map (Item : Record_Wrapper) return Record_Map_Reference;

private
   type Record_Wrapper is new Record_Interface
   with
      record
         Map : aliased Record_Map;
      end record;

end Protypo.Api.Engine_Values.Record_Wrappers;
