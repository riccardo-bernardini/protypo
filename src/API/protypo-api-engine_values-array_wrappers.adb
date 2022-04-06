pragma Ada_2012;
package body Protypo.Api.Engine_Values.Array_Wrappers is
   type Wrapper_Access is access Array_Wrapper;

   ------------------
   -- Make_Wrapper --
   ------------------

   function Make_Wrapper (Init : Array_Type)
                          return Handlers.Ambivalent_Interface_Access
   is
      Result : constant Wrapper_Access :=
                 new Array_Wrapper'(A => Engine_Value_Array_Wrappers.Make_Wrapper);
   begin
      for Idx in Init'Range loop
         Result.Set (Index => Idx,
                     Value => Init (Idx));
      end loop;

      return Handlers.Ambivalent_Interface_Access (Result);
   end Make_Wrapper;

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Array_Wrapper;
                     Value     : Element_Type)
   is
   begin
      Container.A.Append (Create (Value));
   end Append;

   ---------
   -- Set --
   ---------

   procedure Set
     (Container : in out Array_Wrapper; Index : Index_Type;
      Value     :        Element_Type)
   is
   begin
      Container.A.Set (Index => Index,
                       Value => Create (Value));
   end Set;


end Protypo.Api.Engine_Values.Array_Wrappers;
