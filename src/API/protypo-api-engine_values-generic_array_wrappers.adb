pragma Ada_2012;
package body Protypo.Api.Engine_Values.Generic_Array_Wrappers is
   type Wrapper_Access is access Array_Wrapper;

   ------------------
   -- Make_Wrapper --
   ------------------

   function Make_Wrapper (Init : Array_Type) return Ambivalent_Interface_Access
   is
      Result : constant Wrapper_Access := new Array_Wrapper;
   begin
      for Idx in Init'Range loop
         Result.Set (Index => Idx,
                     Value => Create (Init (Idx)));
      end loop;

      return Ambivalent_Interface_Access (Result);
   end Make_Wrapper;


   ---------
   -- Set --
   ---------

   procedure Set
     (Container : in out Array_Wrapper; Index : Index_Type;
      Value     :        Engine_Value)
   is
   begin
      Container.A.Set (Index => Adjust (Index),
                       Value => Value);
   end Set;


end Protypo.Api.Engine_Values.Generic_Array_Wrappers;
