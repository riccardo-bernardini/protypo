pragma Ada_2012;
package body Protypo.Code_Trees.Interpreter.Symbol_Table_References is
   use Symbol_Tables.Protypo_Tables;

   ----------
   -- Read --
   ----------

   function Read (X : Symbol_Reference)
                  return Engine_Values.Engine_Value is
   begin
      return Symbols.Get_Value (Value (X.Position));
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (What  : Symbol_Reference;
                    Value : Engine_Values.Engine_Value) is
   begin
      Update (Pos       => What.Position,
              New_Value => Symbols.To_Symbol_Value (Value));
   end Write;

   ----------------------------
   -- Symbol_Table_Reference --
   ----------------------------

   function Symbol_Table_Reference (Position : Symbol_Tables.Protypo_Tables.Cursor)
                                    return Symbol_Reference
   is
   begin
      if Position = Symbol_Tables.Protypo_Tables.No_Element then
         raise Constraint_Error;
      end if;

      if Symbol_Tables.Class_Of (Position) /= Symbols.Engine_Value_Class then
         raise Constraint_Error;
      end if;

      return Symbol_Reference'(Position => Position);
   end Symbol_Table_Reference;

end Protypo.Code_Trees.Interpreter.Symbol_Table_References;
