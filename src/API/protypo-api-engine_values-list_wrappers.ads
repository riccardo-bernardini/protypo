with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;

package Protypo.Api.Engine_Values.List_Wrappers is
   type List is tagged private;

   procedure Append (Item  : in out List;
                     Value : Engine_Value);

   function Iterator (Item : List) return Iterator_Interface_Access;
private
   package Engine_Value_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Engine_Value);

   type Value_List_Access is access Engine_Value_Lists.List;
   type List is new Ada.Finalization.Controlled with
      record
         L : Value_List_Access;
      end record;

   overriding procedure Initialize (Obj : in out List);
end Protypo.Api.Engine_Values.List_Wrappers;
