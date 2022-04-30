pragma Ada_2012;
package body Protypo.Symbols is

   ---------------------
   -- To_Symbol_Value --
   ---------------------

   function To_Symbol_Value
     (Item : Api.Engine_Values.Engine_Value) return Engine_Value_Type
   is
   begin
      return Symbol_Value'(Class => Engine_Value_Class,
                           Val   => Value_Holders.To_Holder (Item));
   end To_Symbol_Value;

   function Get_Value
     (Item : Engine_Value_Type)
      return Api.Engine_Values.Engine_Value
   is (Value_Holders.Element (Item.Val));


   ---------------------
   -- To_Symbol_Value --
   ---------------------

   function To_Symbol_Value
     (Item : Api.Engine_Values.Handlers.Function_Interface'Class)
      return Function_Type
   is
   begin
      return Symbol_Value'(Class => Function_Class,
                           Fun   => Function_Holders.To_Holder (Item));
   end To_Symbol_Value;


   function Get_Function
     (Item : Function_Type)
      return Api.Engine_Values.Handlers.Function_Interface'Class
   is (Function_Holders.Element (Item.Fun));


   ---------------------
   -- To_Symbol_Value --
   ---------------------

   function To_Symbol_Value
     (Item : Api.Engine_Values.Handlers.Procedure_Interface'Class)
      return Procedure_Type
   is
   begin
      return Symbol_Value'(Class => Procedure_Class,
                           Proc  => Procedure_Holders.To_Holder (Item));

   end To_Symbol_Value;


   function Get_Procedure
     (Item : Procedure_Type)
      return Api.Engine_Values.Handlers.Procedure_Interface'Class
   is (Procedure_Holders.Element (Item.Proc));

end Protypo.Symbols;
