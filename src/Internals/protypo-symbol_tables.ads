with Symbol_Tables.Generic_Symbol_Table;
with Protypo.Api.Engine_Values.Handlers;

with Protypo.Symbols;

private package Protypo.Symbol_Tables is
   --use type Symbols.Symbol_Name;
   use Protypo.Api;

   package Protypo_Tables is
     new Standard.Symbol_Tables.Generic_Symbol_Table
       (Symbol_Name      => Symbols.Symbol_Name,
        Symbol_Value     => Symbols.Symbol_Value,
        Hash             => Symbols.Hash,
        Equivalent_Names => Symbols.Equivalent);

   use type Protypo_Tables.Cursor;

   type Symbol_Table_Type is new Protypo_Tables.Symbol_Table with null record;

   --  function Copy_Globals (X : Table) return Table
   --                         renames Protypo_Tables.Copy_Globals;

   procedure Define_Variable
     (Where : in out Symbol_Table_Type;
      Name  : Id;
      Value : Engine_Values.Engine_Value);

   procedure Define_Function
     (Where : in out Symbol_Table_Type;
      Name  : Id;
      Funct : Engine_Values.Handlers.Function_Interface'Class);

   procedure Define_Procedure
     (Where : in out Symbol_Table_Type;
      Name  : Id;
      Proc  : Engine_Values.Handlers.Procedure_Interface'Class);

   procedure Define_Procedure
     (Where    : in out Symbol_Table_Type;
      Name     : Id;
      Proc     : Engine_Values.Handlers.Procedure_Interface'Class;
      Position : out Protypo_Tables.Cursor);

   function Class_Of (Pos : Protypo_Tables.Cursor)
                      return Symbols.Symbol_Value_Class
     with
       Pre => Pos /= Protypo_Tables.No_Element;
end Protypo.Symbol_Tables;
