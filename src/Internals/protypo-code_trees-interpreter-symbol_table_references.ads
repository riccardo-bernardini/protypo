package Protypo.Code_Trees.Interpreter.Symbol_Table_References is
   type Symbol_Reference is new Reference_Interface with private;

   function Read (X : Symbol_Reference) return Engine_Value;

   procedure Write (What  : Symbol_Reference;
                    Value : Engine_Value);

   function Symbol_Table_Reference (Position : Api.Symbols.Protypo_Tables.Cursor)
                                    return Reference_Interface_Access;
private
   type Symbol_Reference is new Reference_Interface with
      record
         Position : Api.Symbols.Protypo_Tables.Cursor;
      end record;


   function Symbol_Table_Reference (Position : Api.Symbols.Protypo_Tables.Cursor)
                                    return Reference_Interface_Access
   is (new Symbol_Reference'(Position => Position));
end Protypo.Code_Trees.Interpreter.Symbol_Table_References;
