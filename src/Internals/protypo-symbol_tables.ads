with Symbol_Tables.Generic_Symbol_Table;

with Protypo.Symbols;

private package Protypo.Symbol_Tables is
   use type Symbols.Symbol_Name;

   package Protypo_Tables is
     new Standard.Symbol_Tables.Generic_Symbol_Table
       (Symbol_Name      => symbols.Symbol_Name,
        Symbol_Value     => Symbols.Symbol_Value,
        Hash             => symbols.Hash,
        Equivalent_Names => Symbols.Equivalent);

   use type Protypo_Tables.Cursor;

   subtype Table is Protypo_Tables.Symbol_Table;

   function Copy_Globals (X : Table) return Table
                          renames Protypo_Tables.Copy_Globals;

   function Class_Of (Pos : Protypo_Tables.Cursor)
                      return Symbols.Symbol_Value_Class
     with
       Pre => Pos /= Protypo_Tables.No_Element;
end Protypo.Symbol_Tables;
