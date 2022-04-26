with Protypo.Api.Engine_Values;

--
-- A "Symbol Table Reference" is a descendant of Reference_Interface
-- that allows reading/writing a value from/to the symbol table
--
package Protypo.Code_Trees.Interpreter.Symbol_Table_References is
   use Protypo.Api;
   use type Protypo.Api.Symbols.Protypo_Tables.Cursor;

   type Symbol_Reference is
     new Engine_Values.Engine_Reference
   with
     private;

   function Symbol_Table_Reference
     (Position : Symbols.Protypo_Tables.Cursor)
      return Symbol_Reference
     with
       Pre => Position /= Symbols.Protypo_Tables.No_Element;
   -- Get the reference associated with the specified table cursor

   overriding function Read (X : Symbol_Reference)
                             return Engine_Values.Engine_Value;

   overriding procedure Write (What  : Symbol_Reference;
                               Value : Engine_Values.Engine_Value);

   overriding function Is_Writable (Item : Symbol_Reference) return Boolean;

private
   type Symbol_Reference is
     new Engine_Values.Engine_Reference
   with
      record
         Position : Symbols.Protypo_Tables.Cursor;
      end record
     with
       Type_Invariant =>
         Symbol_Reference.Position /= Symbols.Protypo_Tables.No_Element;


   function Is_Writable (Item : Symbol_Reference) return Boolean
   is (True);

end Protypo.Code_Trees.Interpreter.Symbol_Table_References;
