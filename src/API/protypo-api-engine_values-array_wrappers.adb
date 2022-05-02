pragma Ada_2012;
package body Protypo.Api.Engine_Values.Array_Wrappers is

   ------------------
   -- Make_Wrapper --
   ------------------

   function Make_Wrapper (Init : Array_Type)
                          return Handlers.Ambivalent_Interface_Access
   is
      Tmp    : Engine_Value_Array;
   begin
      for el of Init loop
         Tmp.Append (Create (El));
      end loop;

      return Engine_Value_Array_Wrappers.Make_Wrapper (Tmp);
   end Make_Wrapper;

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Array_Wrapper;
                     Value     : Element_Type)
   is
   begin
      Container.Append (Create (Value));
   end Append;

   ---------
   -- Set --
   ---------

   procedure Set
     (Container : in out Array_Wrapper; Index : Index_Type;
      Value     :        Element_Type)
   is
   begin
      Container.Set (Index => Index,
                     Value => Create (Value));
   end Set;


end Protypo.Api.Engine_Values.Array_Wrappers;
