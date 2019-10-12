with Ada.Strings.Hash_Case_Insensitive;
with Ada.Containers;

with Symbol_Tables.Generic_Symbol_Table;
with Protypo.API.Engine_Values;

--
-- This package defines the symbol tables used by the engine.  They
-- are basically structures that map symbol names into Engine_Value's
--
package Protypo.API.Symbols is
   subtype Symbol_Name is ID;

   function Hash (X : Symbol_Name) return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash_Case_Insensitive (String (X)));

   function Equivalent (X, Y : Symbol_Name) return Boolean
   is (X = Y);

   package Protypo_Tables is
     new Symbol_Tables.Generic_Symbol_Table
       (Symbol_Name      => Symbol_Name,
        Symbol_Value     => Engine_Values.Engine_Value,
        Hash             => Hash,
        Equivalent_Names => Equivalent);

   subtype Table is Protypo_Tables.Symbol_Table;

   function Copy_Globals (X : Table) return Table
                  renames Protypo_Tables.Copy_Globals;
end Protypo.API.Symbols;
