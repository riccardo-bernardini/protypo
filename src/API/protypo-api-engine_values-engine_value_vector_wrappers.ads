with Ada.Finalization;
with Protypo.Api.Engine_Values.Handlers;
--
-- ## What is this?
--
-- This is a wrapper that includes in itself a vector of `Engine_Value`.
-- It allows to access the internal vector via the `Vector` function
-- that returns a reference.
--
-- It implements the `Ambivalent_Interface` and it exports the usual
-- access methods
--
-- * _indexed_ access to access a specific element of the array
-- * _first_, _last_ and _length_  analoguos to the corresponding Ada
--   attributes for arrays.
-- * _range_ and _iterate_ iterators to run over the array content.
--   _range_ iterates over the set of indexes, _iterate_ over the array
--   elements
--

package Protypo.Api.Engine_Values.Engine_Value_Vector_Wrappers is
   type Vector_Reference (Ref : access Engine_Value_Array) is limited private
     with Implicit_Dereference => Ref;

   type Vector_Handler is
     new Ada.Finalization.Controlled
     and Handlers.Ambivalent_Interface
   with
     private;

   type Vector_Handler_Access is access Vector_Handler;

   overriding function Get (X     : Vector_Handler;
                            Index : Engine_Value_Array)
                               return Engine_Reference'Class;

   overriding function Get (X     : Vector_Handler;
                            Field : Id)
                            return Engine_Reference'Class;

   overriding function Is_Field (X : Vector_Handler; Field : Id)
                                 return Boolean;

   function Vector (Item : Vector_Handler) return Vector_Reference;


private
   type Vector_Reference (Ref : access Engine_Value_Array) is limited null record;
   type Vector_Access is access Engine_Value_Array;

   type Vector_Handler is
     new Ada.Finalization.Controlled
     and Handlers.Ambivalent_Interface
   with
      record
         Vect : Vector_Access;
      end record;

   overriding procedure Initialize (Object : in out Vector_Handler);

   --     function Make_Handler return Vector_Handler_Access
   --     is (new Vector_Handler'(Vect => new Vectors.Vector'(Vectors.Empty_Vector)));

   function Vector (Item : Vector_Handler) return Vector_Reference
   is (Vector_Reference'(Ref => Item.Vect));

end Protypo.Api.Engine_Values.Engine_Value_Vector_Wrappers;
