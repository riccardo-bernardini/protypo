pragma Ada_2012;
package body Protypo.Api.References.Constant_References is

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

end Protypo.Api.References.Constant_References;
