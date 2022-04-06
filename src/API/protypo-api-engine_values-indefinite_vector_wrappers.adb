pragma Ada_2012;

package body Protypo.Api.Engine_Values.Indefinite_Vector_Wrappers is

   ------------------
   -- Make_Wrapper --
   ------------------

   function Make_Wrapper
     (Init : Element_Vectors.Vector)
      return Handlers.Ambivalent_Interface_Access
   is
      Val : Engine_Value_Vectors.Vector;
   begin
      for Element of Init loop
         Val.Append (Create (Element));
      end loop;

      return new Array_Wrapper'(A => Engine_Value_Array_Wrappers.Make_Wrapper (Val));
   end Make_Wrapper;


   ---------
   -- Set --
   ---------

   procedure Set
     (Container : in out Array_Wrapper;
      Index     : Index_Type;
      Value     : Element_Type)
   is
   begin
      Container.A.Set (Integer (Index), Create (Value));
   end Set;

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Array_Wrapper; Value : Element_Type) is
   begin
      Container.A.Append (Create (Value));
   end Append;




end Protypo.Api.Engine_Values.Indefinite_Vector_Wrappers;
