with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Finalization;

use Ada;

generic
   type Symbol_Name is new String;
   type Symbol_Value (<>) is private;
   with function Hash (Key : Symbol_Name) return Ada.Containers.Hash_Type;
   with function Equivalent_Names (X, Y : Symbol_Name) return Boolean;
package Symbol_Tables.Generic_Symbol_Table is
   type Symbol_Table is
     new Finalization.Limited_Controlled
   with
     private;

   type Cursor is private;
   No_Element : constant Cursor;

   type Table_Block is private;
   No_Block : constant Table_Block;

   function Copy_Globals (T : Symbol_Table) return Symbol_Table;

   function Root (T : Symbol_Table) return Table_Block;

   function Current_Block (T : Symbol_Table) return Table_Block;

   function Parent_Of (T     : Symbol_Table;
                       Block : Table_Block) return Table_Block
     with Post => (if Block = T.Root then Parent_Of'Result = No_Block);


   procedure Open_Block (Table  : in out Symbol_Table;
                         Parent : Table_Block)
     with
       Pre => Parent /= No_Block,
       Post => Parent_Of (Table, Table.Current_Block) = Parent;

   procedure Open_internal_Block (Table : in out Symbol_Table);
   -- Syntactic sugar: Parent defaults to Table.Current_Block

   procedure Open_External_Block (Table : in out Symbol_Table);
   -- Syntactic sugar: Parent defaults to Table.Root

   procedure Close_Block (Table : in out Symbol_Table)
     with
       Pre => Table.Current_Block /= Table.Root;


   function Find (Table : Symbol_Table;
                  Name  : Symbol_Name)
                  return Cursor;

   function Contains (Table : Symbol_Table;
                      Name  : Symbol_Name)
                      return Boolean
   is (Table.Find (Name) /= No_Element);

   function Value (Pos : Cursor) return Symbol_Value;

   function Contains (Block : Table_Block;
                      Name  : Symbol_Name)
                      return Boolean;

   procedure Create (Table         : in out Symbol_Table;
                     Name          : Symbol_Name;
                     Initial_Value : Symbol_Value)
     with
       Pre =>
         not Contains (Table.Current_Block, Name),
     Post =>
       Contains (Table.Current_Block, Name)
     and
       Table.Current_Block = Table.Current_Block'Old;

   procedure Update (Pos       : Cursor;
                     New_Value : Symbol_Value)
     with
       Pre => Pos /= No_Element;
private
   package Symbol_Maps is
     new  Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => Symbol_Name,
                                                 Element_Type    => Symbol_Value,
                                                 Hash            => Hash,
                                                 Equivalent_Keys => Equivalent_Names);



   type Basic_Block;

   type Table_Block is access Basic_Block;
   No_Block : constant Table_Block := null;

   type Basic_Block is
      record
         Map         : Symbol_Maps.Map;
         Parent      : Table_Block;
         Old_Current : Table_Block;
      end record;

   type Cursor is
      record
         Block           : Table_Block;
         Internal_Cursor : Symbol_Maps.Cursor;
      end record;

   No_Element : constant Cursor := Cursor'(Block           => No_Block,
                                           Internal_Cursor => Symbol_Maps.No_Element);

   type Symbol_Table is
     new Finalization.Limited_Controlled
   with
      record
         Root    : Table_Block;
         Current : Table_Block;
      end record;

   overriding procedure Initialize (Object : in out Symbol_Table);

   function Root (T : Symbol_Table) return Table_Block
   is (T.Root);

   function Current_Block (T : Symbol_Table) return Table_Block
   is (T.Current);

   function Parent_Of (T     : Symbol_Table;
                       Block : Table_Block) return Table_Block
   is (Block.Parent);


   function Contains (Block : Table_Block;
                      Name  : Symbol_Name)
                      return Boolean
   is (Block.Map.Contains (Name));


   function Value (Pos : Cursor) return Symbol_value
   is (Symbol_Maps.Element (Pos.Internal_Cursor));

end Symbol_Tables.Generic_Symbol_Table;
