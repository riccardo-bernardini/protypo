with Protypo.Api.Engine_Values;
with Protypo.Symbol_Tables;
with Protypo.Symbols;

--
-- A "Symbol Table Reference" is a descendant of Reference_Interface
-- that allows reading/writing a value from/to the symbol table
--
package Protypo.Code_Trees.Interpreter.Symbol_Table_References is
   use Protypo.Api;
   use type Protypo.Symbol_Tables.Protypo_Tables.Cursor;
   use type Protypo.Symbols.Symbol_Value_Class;

   type Symbol_Reference is
     new Engine_Values.Engine_Reference
   with
     private;

   function Symbol_Table_Reference
     (Position : Symbol_Tables.Protypo_Tables.Cursor)
      return Symbol_Reference
     with
       Pre =>
         Position /= Symbol_Tables.Protypo_Tables.No_Element
         and then Symbol_Tables.Class_Of (Position) = Symbols.Engine_Value_Class;
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
         Position : Symbol_Tables.Protypo_Tables.Cursor;
      end record
     with
       Type_Invariant =>
         Symbol_Reference.Position /= Symbol_Tables.Protypo_Tables.No_Element;


   function Is_Writable (Item : Symbol_Reference) return Boolean
   is (True);

end Protypo.Code_Trees.Interpreter.Symbol_Table_References;
