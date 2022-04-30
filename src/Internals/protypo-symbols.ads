with Ada.Strings.Hash_Case_Insensitive;
with Ada.Containers.Indefinite_Holders;

with Protypo.Api.Engine_Values.Handlers;

--
-- This package defines the symbol tables used by the engine.  They
-- are basically structures that map symbol names into Engine_Value's
--
private package Protypo.Symbols is
   subtype Symbol_Name is Id;

   type Symbol_Value_Class is
     (
      Engine_Value_Class,
      Function_Class,
      Procedure_Class
     );

   type Symbol_Value (Class : Symbol_Value_Class) is private;

   subtype Engine_Value_Type is Symbol_Value (Engine_Value_Class);
   subtype Function_Type is Symbol_Value (Function_Class);
   subtype Procedure_Type is Symbol_Value (Procedure_Class);

   function To_Symbol_Value
     (Item : Api.Engine_Values.Engine_Value)
      return Engine_Value_Type;

   function Get_Value
     (Item : Engine_Value_Type)
      return Api.Engine_Values.Engine_Value;

   function To_Symbol_Value
     (Item : Api.Engine_Values.Handlers.Function_Interface'Class)
      return Function_Type;

   function Get_Function
     (Item : Function_Type)
      return Api.Engine_Values.Handlers.Function_Interface'Class;

   function To_Symbol_Value
     (Item : Api.Engine_Values.Handlers.Procedure_Interface'Class)
      return Procedure_Type;


   function Get_Procedure
     (Item : Procedure_Type)
      return Api.Engine_Values.Handlers.Procedure_Interface'Class;

   function Hash (X : Symbol_Name) return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash_Case_Insensitive (String (X)));

   function Equivalent (X, Y : Symbol_Name) return Boolean
   is (X = Y);

private
   use Protypo.Api.Engine_Values;
   use type Handlers.Function_Interface;
   use type Handlers.Procedure_Interface;

   package Value_Holders is
     new Ada.Containers.Indefinite_Holders (Engine_Value);

   package Function_Holders is
     new Ada.Containers.Indefinite_Holders (Handlers.Function_Interface'Class);

   package Procedure_Holders is
     new Ada.Containers.Indefinite_Holders (Handlers.Procedure_Interface'Class);

   type Symbol_Value (Class : Symbol_Value_Class) is
      record
         case Class is
            when Engine_Value_Class =>
               Val : Value_Holders.Holder;

            when Function_Class =>
               Fun : Function_Holders.Holder;

            when Procedure_Class =>
               Proc : Procedure_Holders.Holder;
         end case;
      end record;

end Protypo.Symbols;
