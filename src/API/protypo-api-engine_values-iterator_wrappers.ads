with Ada.Iterator_Interfaces;

with Protypo.Api.Engine_Values.Handlers;

generic
   type Cursor is private;

   with function Has_Element (Position : Cursor) return Boolean;

   with function Element (Position : Cursor) return Engine_Value;

   with package Basic_Iterators is
     new Ada.Iterator_Interfaces (Cursor       => Cursor,
                                  Has_Element  => Has_Element);

package Protypo.Api.Engine_Values.Iterator_Wrappers is
   type Iterator_Wrapper is
     new Handlers.Iterator_Interface
   with
     private;

   type Iterator_Wrapper_Access is access Iterator_Wrapper;

   type Basic_Iterator_Access is access all Basic_Iterators.Reversible_Iterator'Class;

   function Make_Wrapper (Iter : Basic_Iterator_Access)
                          return Iterator_Wrapper_Access;

   overriding procedure Reset (Iter : in out Iterator_Wrapper);

   overriding procedure Next (Iter : in out Iterator_Wrapper);

   overriding function End_Of_Iteration (Iter : Iterator_Wrapper) return Boolean;

   overriding function Element (Iter : Iterator_Wrapper) return Engine_Value;

private
   type Iterator_Wrapper is
     new Handlers.Iterator_Interface with
      record
         Iterator : Basic_Iterator_Access;
         Position : Cursor;
      end record;
end Protypo.Api.Engine_Values.Iterator_Wrappers;
