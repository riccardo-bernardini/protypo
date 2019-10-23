--
-- Utility package to convert IDs to enumarative values.
--

private generic
   type Field_Enumerator is (<>);
   Prefix : String := "";
package Protypo.Api.Field_Names is
   function Is_Field (X : ID) return Boolean;

   function To_Field (X : ID) return Field_Enumerator;
end Protypo.Api.Field_Names;
