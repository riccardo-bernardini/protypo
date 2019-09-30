pragma Ada_2012;
package body Protypo.Code_Trees.Interpreter.Symbol_Table_References is
   use Api.Symbols.Protypo_Tables;

   ----------
   -- Read --
   ----------

   function Read (X : Symbol_Reference) return Engine_Value is
   begin
      return Value (X.Position);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (What : Symbol_Reference; Value : Engine_Value) is
   begin
      Update (Pos       => What.Position,
              New_Value => Value);
   end Write;

end Protypo.Code_Trees.Interpreter.Symbol_Table_References;