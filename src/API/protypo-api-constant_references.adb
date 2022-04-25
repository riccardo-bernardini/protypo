pragma Ada_2012;
package body Protypo.Api.Constant_References is
   procedure Write (Ref  : Constant_Reference;
                    Item : Engine_Values.Engine_Value)
   is
   begin
      raise Program_Error with "Trying writing a constant reference";
   end Write;

   ------------------
   -- To_Reference --
   ------------------

   function To_Reference
     (Value : Engine_Values.Engine_Value) return Constant_Reference
   is (Constant_Reference'(Class => Value.Class,
                           Value => Value));

   ----------
   -- Read --
   ----------

   function Read (Ref : Constant_Reference) return Engine_Values.Engine_Value
   is (Ref.Value);

end Protypo.Api.Constant_References;
