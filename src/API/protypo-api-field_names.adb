pragma Ada_2012;
package body Protypo.Api.Field_Names is

   --------------
   -- Is_Field --
   --------------

   function Is_Field (X : String) return Boolean is
      Ignored : Field_Enumerator;
   begin
      Ignored := To_Field (x);
      return True;
      -- Yes, I know, it is not the best practice to use exceptions
      -- to do flow control, but this is the easiest way
   exception
      when Constraint_Error =>
         return False;
   end Is_Field;
   --------------
   -- To_Field --
   --------------

   function To_Field (X : String) return Field_Enumerator
   is (Field_Enumerator'Value (Prefix & X));


end Protypo.Api.Field_Names;
