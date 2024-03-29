with Protypo.Api.Engine_Values.Handlers;
--
-- A "Symbol Table Reference" is a descendant of Reference_Interface
-- that allows reading/writing a value from/to the symbol table
--
package Protypo.Code_Trees.Interpreter.Symbol_Table_References is
   use Protypo.Api;

   type Symbol_Reference is
     new Engine_Values.Handlers.Reference_Interface
   with
     private;

   function Symbol_Table_Reference
     (Position : Api.Symbols.Protypo_Tables.Cursor)
      return Engine_Values.Handlers.Reference_Interface_Access;
   -- Get the reference associated with the specified table cursor

   overriding function Read (X : Symbol_Reference) return Engine_Values.Engine_Value;

   overriding procedure Write (What  : Symbol_Reference;
                               Value : Engine_Value);

private
   type Symbol_Reference is
     new Engine_Values.Handlers.Reference_Interface
       with
      record
         Position : Api.Symbols.Protypo_Tables.Cursor;
      end record;


  end Protypo.Code_Trees.Interpreter.Symbol_Table_References;
