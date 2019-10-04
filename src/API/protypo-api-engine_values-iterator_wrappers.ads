with Ada.Iterator_Interfaces;

generic
   type Cursor is private;

   with function Has_Element (Position : Cursor) return Boolean;

   with function Element (Position : Cursor) return Engine_Value;

   with package Iterators is
     new Ada.Iterator_Interfaces (Cursor       => Cursor,
                                  Has_Element  => Has_Element);

package Protypo.Api.Engine_Values.Iterator_Wrappers is
   type Iterator_Wrapper is
     new Iterator_Interface with private;

   type Iterator_Wrapper_Access is access Iterator_Wrapper;

   type Basic_Iterator_Access is access all Iterators.Forward_Iterator'Class;

   function Make_Wrapper (Iter : Basic_Iterator_Access)
                          return Iterator_Wrapper_Access;

   procedure Reset (Iter : in out Iterator_Wrapper);

   procedure Next (Iter : in out Iterator_Wrapper);

   function End_Of_Iteration (Iter : Iterator_Wrapper) return Boolean;

   function Element (Iter : Iterator_Wrapper) return Engine_Value;

private
   type Iterator_Wrapper is
     new Iterator_Interface with
      record
         Iterator : Basic_Iterator_Access;
         Position : Cursor;
      end record;
end Protypo.Api.Engine_Values.Iterator_Wrappers;
