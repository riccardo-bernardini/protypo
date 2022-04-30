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

   ---------------------
   -- Define_Variable --
   ---------------------

   procedure Define_Variable
     (Where : in out Symbol_Table_type;
      Name  : Id;
      Value : Engine_Values.Engine_Value)
   is
   begin
      Where.Create (Name, Symbols.To_Symbol_Value (Value));
   end Define_Variable;

   ---------------------
   -- Define_Function --
   ---------------------

   procedure Define_Function
     (Where : in out Symbol_Table_type;
      Name  : Id;
      Funct : Engine_Values.Handlers.Function_Interface'Class)
   is
   begin
      Where.Create (Name, Symbols.To_Symbol_Value (Funct));
   end Define_Function;

   ----------------------
   -- Define_Procedure --
   ----------------------

   procedure Define_Procedure
     (Where : in out Symbol_Table_type;
      Name  : Id;
      Proc  : Engine_Values.Handlers.Procedure_Interface'Class)
   is
   begin
      Where.Create (Name, Symbols.To_Symbol_Value (Proc));
   end Define_Procedure;

   ----------------------
   -- Define_Procedure --
   ----------------------

   procedure Define_Procedure
     (Where    : in out Symbol_Table_Type;
      Name     : Id;
      Proc     : Engine_Values.Handlers.Procedure_Interface'Class;
      Position : out Protypo_Tables.Cursor)
   is
   begin
      Where.Create (Name, Symbols.To_Symbol_Value (Proc), Position);
   end Define_Procedure;



end Protypo.Symbol_Tables;
