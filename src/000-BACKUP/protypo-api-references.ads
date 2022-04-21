with Protypo.Api.Engine_Values;

package Protypo.Api.References is
   type Reference is interface;

   type Reference_Access is access all Reference;

   function Read (Ref : Reference) return Engine_Values.Engine_Value
                  is abstract;

   type Writable_Reference is interface and Reference;

   procedure Write (Ref       : Writable_Reference;
                    New_Value : Engine_Values.Engine_Value)
   is abstract;
end Protypo.Api.References;
