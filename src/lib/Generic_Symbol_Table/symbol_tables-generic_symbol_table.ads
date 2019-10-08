with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Holders;
with Ada.Finalization;

with Ada.Strings.Unbounded;
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

   function Image (X : Cursor) return String;

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

   procedure Open_Internal_Block (Table : in out Symbol_Table);
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

   function Has_Value (Pos : Cursor) return Boolean
     with Pre => Pos /= No_Element;

   function Value (Pos : Cursor) return Symbol_Value
     with Pre => Has_Value (Pos);


   function Name (Pos : Cursor) return Symbol_Name
     with Pre => Pos /= No_Element;

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

   procedure Create (Table         : in out Symbol_Table;
                     Name          : Symbol_Name;
                     Initial_Value : Symbol_Value;
                     Position      : out Cursor)
     with
       Pre =>
         not Contains (Table.Current_Block, Name),
     Post =>
       Contains (Table.Current_Block, Name)
     and
       Table.Current_Block = Table.Current_Block'Old
       and
         Table.Find (Name) = Position
     and
       Has_Value (Position)
     and
       Value (Position) = Initial_Value;

   procedure Create (Table         : in out Symbol_Table;
                     Name          : Symbol_Name;
                     Position      : out Cursor)
     with
       Pre =>
         not Contains (Table.Current_Block, Name),
     Post =>
       Contains (Table.Current_Block, Name)
     and
       Table.Current_Block = Table.Current_Block'Old
       and
         Table.Find (Name) = Position
     and
       not Has_Value (Position);

   procedure Update (Pos       : Cursor;
                     New_Value : Symbol_Value)
     with
       Pre => Pos /= No_Element;

   Uninitialized_Value : exception;

   type Value_Printer is access function (X : Symbol_Value) return String;

   procedure Set_Printer (Callback : Value_Printer);
   -- Useful for debugging.  Setup a function that converts symbol values
   -- to string.  This function will be used in generating debug prints.
   -- Why specifying the converter in this way  and not as a parameter
   -- to package?  Because it is a feature that it is not always
   -- required.

private
   --
   -- A symbol table is made of several blocks, ordered
   --
   -- Every block is organized with a two-step structure: the values
   -- are stored into a vector, while we store the map key-> internal
   -- index in a map.  Why this complication?  Because the pair
   -- (block, index) is a stable cursor to the element that is not affected
   -- by insertions.
   --
   use Ada.Strings.Unbounded;

   package Value_Holders is
     new Ada.Containers.Indefinite_Holders (Symbol_Value);

   use type Value_Holders.Holder;

   subtype Map_Entry is Value_Holders.Holder;

   package Symbol_Maps is
     new  Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => Symbol_Name,
                                                 Element_Type    => Map_Entry,
                                                 Hash            => Hash,
                                                 Equivalent_Keys => Equivalent_Names);



   type Basic_Block;

   type Table_Block is access Basic_Block;
   No_Block : constant Table_Block := null;

   subtype Block_ID is Positive;

   type Basic_Block is
      record
         Map         : Symbol_Maps.Map;
         Parent      : Table_Block;
         Old_Current : Table_Block;
         ID          : Block_ID;
      end record;

   type Cursor (Valid : Boolean := True) is
      record
         case Valid is
            when True =>
               Block : Table_Block;
               Key   : Unbounded_String;

            when False =>
               null;
         end case;
      end record;

   No_Element : constant Cursor := Cursor'(Valid => False);


   function Image (X : Cursor) return String
   is (if X.Valid then
          "[" & To_String (X.Key) & "@" & X.Block.ID'Image & "]"
       else
          "NO_ELEMENT");

   type Symbol_Table is
     new Finalization.Limited_Controlled
   with
      record
         Root    : Table_Block;
         Current : Table_Block;
         Counter : Block_ID;
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


   function Value (Pos : Cursor) return Symbol_Value
   is (if Has_Value (Pos) then
          Pos.Block.Map.Element (Symbol_Name (To_String (Pos.Key))).Element
       else
          raise Uninitialized_Value with String (Name (Pos)));

   function Has_Value (Pos : Cursor) return Boolean
   is (Pos.Valid);

   function Name (Pos : Cursor) return Symbol_Name
   is (Symbol_Name(To_String(Pos.Key)));


end Symbol_Tables.Generic_Symbol_Table;
