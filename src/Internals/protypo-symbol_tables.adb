pragma Ada_2012;
package body Protypo.Symbol_Tables is

   --------------
   -- Class_Of --
   --------------

   function Class_Of
     (Pos : Protypo_Tables.Cursor) return Symbols.Symbol_Value_Class
   is
   begin
      if Pos = Protypo_Tables.No_Element then
         raise Constraint_Error;
      end if;

      return Protypo_Tables.Value (Pos).Class;
   end Class_Of;

end Protypo.Symbol_Tables;
