with Ada.Containers.Vectors;
with Ada.Finalization;

generic
   type Index_Type is range <>;

package Protypo.Api.Engine_Values.Value_Vectors is
   package Vectors is
     new Ada.Containers.Vectors (Index_Type   => Index_Type,
                                 Element_Type => Engine_Value);

   type Vector_Handler is
     new Ada.Finalization.Controlled
     and Ambivalent_Interface
   with
     private;

   type Vector_Handler_Access is access Vector_Handler;

--     function Make_Handler return Vector_Handler_Access;

   type Vector_Reference (Ref : access Vectors.Vector) is limited private
     with Implicit_Dereference => Ref;

   function Vector (Item : Vector_Handler) return Vector_Reference;


   function Get (X     : Vector_Handler;
                 Index : Engine_Value_Array)
                 return Handler_Value;

   function Get (X     : Vector_Handler;
                 Field : ID)
                 return Handler_Value;

   function Is_Field (X : Vector_Handler; Field : Id) return Boolean;

private
   type Vector_Reference (Ref : access Vectors.Vector) is limited null record;
   type Vector_Access is access Vectors.Vector;

   type Vector_Handler is
     new Ada.Finalization.Controlled
     and Ambivalent_Interface
   with
      record
         Vect : Vector_Access;
      end record;

   overriding procedure Initialize(Object : in out Vector_Handler);

--     function Make_Handler return Vector_Handler_Access
--     is (new Vector_Handler'(Vect => new Vectors.Vector'(Vectors.Empty_Vector)));

   function Vector (Item : Vector_Handler) return Vector_Reference
   is (Vector_Reference'(Ref => Item.Vect));

end Protypo.Api.Engine_Values.Value_Vectors;
