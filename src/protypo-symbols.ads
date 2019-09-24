with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Containers;

with Symbol_Tables.Generic_Symbol_Table;
with Protypo.Engine_Values;

package Protypo.Symbols is
   subtype Symbol_Name is String;

   function Hash (X : Symbol_Name) return Ada.Containers.Hash_Type
                  renames Ada.Strings.Hash_Case_Insensitive;

   function Equivalent (X, Y : Symbol_Name) return Boolean
                        renames Ada.Strings.Equal_Case_Insensitive;

   package Protypo_Tables is
     new Symbol_Tables.Generic_Symbol_Table
       (Symbol_Name      => Symbol_Name,
        Symbol_Value     => Engine_Values.Engine_Value,
        Hash             => Hash,
        Equivalent_Names => Equivalent);

   subtype Table is Protypo_Tables.Symbol_Table;

end Protypo.Symbols;
