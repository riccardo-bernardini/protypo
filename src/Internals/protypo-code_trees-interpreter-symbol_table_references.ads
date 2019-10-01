--
-- A "Symbol Table Reference" is a descendant of Reference_Interface
-- that allows reading/writing a value from/to the symbol table
--
package Protypo.Code_Trees.Interpreter.Symbol_Table_References is
   type Symbol_Reference is new Reference_Interface with private;

   function Symbol_Table_Reference
     (Position : Api.Symbols.Protypo_Tables.Cursor)
      return Reference_Interface_Access;
   -- Get the reference associated with the specified table cursor

   overriding function Read (X : Symbol_Reference) return Engine_Value;

   overriding procedure Write (What  : Symbol_Reference;
                               Value : Engine_Value);

private
   type Symbol_Reference is new Reference_Interface with
      record
         Position : Api.Symbols.Protypo_Tables.Cursor;
      end record;


   function Symbol_Table_Reference (Position : Api.Symbols.Protypo_Tables.Cursor)
                                    return Reference_Interface_Access
   is (new Symbol_Reference'(Position => Position));
end Protypo.Code_Trees.Interpreter.Symbol_Table_References;
