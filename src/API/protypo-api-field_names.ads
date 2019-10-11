private

generic
   type Field_Enumerator is (<>);
   Prefix : String := "";
package Protypo.Api.Field_Names is
   function Is_Field (X : String) return Boolean;

   function To_Field (X : String) return Field_Enumerator;
end Protypo.Api.Field_Names;
