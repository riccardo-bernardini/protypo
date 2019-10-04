pragma Ada_2012;
with Protypo.Api.Engine_Values.Constant_Wrappers; use Protypo.Api.Engine_Values.Constant_Wrappers;
--  with Protypo.Api.Engine_Values.Iterator_Wrappers;

package body Protypo.Api.Engine_Values.List_Wrappers is
--     package Wrappers is
--       new Protypo.Api.Engine_Values.Iterator_Wrappers
--         (Cursor      => Engine_Value_Lists.Cursor,
--          Has_Element => Engine_Value_Lists.Has_Element,
--          Element     => Engine_Value_Lists.Element,
--          Iterators   => Engine_Value_Lists.List_Iterator_Interfaces);

   type Iterator_Type is new Iterator_Interface
     with
      record
         Container : Value_List_Access;
         Pos       : Engine_Value_Lists.Cursor;
      end record;

   overriding procedure Reset (Iter : in out Iterator_Type);
   overriding procedure Next (Iter : in out Iterator_type);
   overriding function End_Of_Iteration (Iter : Iterator_type) return Boolean;
   overriding function Element (Iter : Iterator_type) return Handler_Value;

   procedure Reset (Iter : in out Iterator_Type)
   is
   begin
      Iter.Pos := Iter.Container.First;
   end Reset;

   procedure Next (Iter : in out Iterator_type)
   is
   begin
      Engine_Value_Lists.Next (Iter.Pos);
   end Next;

   function End_Of_Iteration (Iter : Iterator_type) return Boolean
   is (not Engine_Value_Lists.Has_Element (Iter.Pos));

   function Element (Iter : Iterator_type) return Handler_Value
   is (To_Handler_Value (Engine_Value_Lists.Element (Iter.Pos)));



   ------------
   -- Append --
   ------------

   procedure Append
     (Item  : in out List;
      Value : Engine_Value)
   is
   begin
      Item.L.Append(Value);
   end Append;

   --------------
   -- Iterator --
   --------------

   function Iterator (Item : List) return Iterator_Interface_Access is
      use Engine_Value_Lists.List_Iterator_Interfaces;

      Tmp : constant Wrappers.Basic_Iterator_Access :=
              new Reversible_Iterator'Class'(Item.L.Iterate);
   begin
      return Iterator_Interface_Access (Wrappers.Make_Wrapper (Tmp));
   end Iterator;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Obj : in out List)
   is
   begin
      Obj.L := new Engine_Value_Lists.List;
   end Initialize;
end Protypo.Api.Engine_Values.List_Wrappers;
