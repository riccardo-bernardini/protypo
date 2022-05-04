pragma Ada_2012;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package body Protypo.Api.Engine_Values.Array_Wrappers is

   ------------------
   -- Make_Wrapper --
   ------------------

   function Make_Wrapper (Init : Array_Type)
                          return Handlers.Ambivalent_Interface_Access
   is
      Tmp    : Engine_Value_Array;
   begin
      for El of Init loop
         Tmp.Append (Create (El));
      end loop;

      return Handlers.Ambivalent_Interface_Access (Engine_Vector_Handlers.To_Vector_Handler (Tmp));
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
      use Engine_Vector_Handlers;
   begin
      Vector_Handler (Container).Set (Index => Index_Type'Pos (Index),
                                      Value => Create (Value));
   end Set;


   overriding function Type_Name (Item : Array_Wrapper) return String
   is (Name);

   overriding function Image (Item   : Array_Wrapper;
                              Format : String := "") return String
   is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      Append (Result, "[");

      for I in Item.First_Index .. Item.Last_Index loop
         declare
            Element : constant Element_Type := Get_Value (Item.Get_Element (I));
            img     : constant string := Image (Element, Format);
         begin
            Append (Result, Img);
         end;

         if I < Item.Last_Index then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, "]");

      return To_String (Result);
   end Image;


end Protypo.Api.Engine_Values.Array_Wrappers;
