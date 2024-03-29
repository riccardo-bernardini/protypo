with Ada.Containers.Indefinite_Hashed_Maps;
--  with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Finalization;

use Ada;

--
-- ## What is this?
--
-- This package implements a generic "symbol table," that is a structure that
-- maps symbol "names" (a string) to symbol values.
--
-- The peculiarity that differentiate the symbol tables defined in this
-- package from an ordirary `Hashed_Maps.Map` is the possibility of
-- having different namespaces, possibly nested.
--
-- ### Data model
--
-- The abstract model is as follows.
-- * The symbol table contains one or more _namespace_
-- * A newly created table has only one namespace: the _root namespace_
--   that contains all the globally visible symbols
-- * At any time there is a _current_ namespace
-- * New namespaces can be created as _children_ of an existing namespace.
--   Two common choices for the parent namespace are
--   * The current namespace (like using a `declare .. begin .. end` in Ada)
--   * The root namespace (like when a new procedure is defined)
-- * When a symbol is searched  for, first it is searched the current
---  namespace.  If the symbol is not found, it is searched in the parent,
--   grand-parent and so on... until the root namespace is reached.
-- * When a new namespace is created it becomes the current one;
--   when the namespace is closed, the previous current namespace is selected.
--
-- It turns out that namespaces are organized in two structures
--
-- * They are organized as a tree (having the global namspace as root)
--   according to the child-parent relationship
-- * They are organized as a stack whose top is the current namespace.
--   New namespaces are pushed to the top and they are popped when
--   they are closed.
--
-- ### Cursors
--
-- When the table is queried it returns a `Cursor` that "points to" the
-- found element (or it assumes the special value `No_Element`).  This
-- is similar to the behaviour of standard Ada containers.
--
-- The main difference with the usual `Cursor` is that the cursor of
-- this structure is "stable" with respect to tampering; that is, it
-- remains valid even if new elements and/or new namespaces are added
-- (I do not think this is guaranteed with the standard containers).
-- Only removing the namespace that contains the entry pointed by the cursors
-- invalidates the cursor (obviously).
--

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
   -- Return a printable representation of a cursor. Useful
   -- mostly for debugging

   type Table_Namespace is private;
   No_Namespace : constant Table_Namespace;

   function Copy_Globals (T : Symbol_Table) return Symbol_Table;

   function Root (T : Symbol_Table) return Table_Namespace;
   -- Return the global namespace of the table

   function Current_Namespace (T : Symbol_Table) return Table_Namespace;
   -- Return the current namespace

   function Parent_Of (T     : Symbol_Table;
                       Block : Table_Namespace) return Table_Namespace
     with Post => (if Block = T.Root then Parent_Of'Result = No_Namespace);
   -- Return the parent of a given namespace


   procedure Open_Namespace (Table  : in out Symbol_Table;
                         Parent : Table_Namespace)
     with
       Pre => Parent /= No_Namespace,
       Post => Parent_Of (Table, Table.Current_Namespace) = Parent;
   -- Create a new namespace with the given parent

   procedure Open_Internal_Namespace (Table : in out Symbol_Table);
   -- Syntactic sugar: Parent defaults to Table.Current_Block

   procedure Open_External_Namespace (Table : in out Symbol_Table);
   -- Syntactic sugar: Parent defaults to Table.Root

   procedure Close_Namespace (Table : in out Symbol_Table)
     with
       Pre => Table.Current_Namespace /= Table.Root;


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

   function Contains (Block : Table_Namespace;
                      Name  : Symbol_Name)
                      return Boolean;

   procedure Create (Table         : in out Symbol_Table;
                     Name          : Symbol_Name;
                     Initial_Value : Symbol_Value)
     with
       Pre =>
         not Contains (Table.Current_Namespace, Name),
     Post =>
       Contains (Table.Current_Namespace, Name)
     and
       Table.Current_Namespace = Table.Current_Namespace'Old;

   procedure Create (Table         : in out Symbol_Table;
                     Name          : Symbol_Name;
                     Initial_Value : Symbol_Value;
                     Position      : out Cursor)
     with
       Pre =>
         not Contains (Table.Current_Namespace, Name),
     Post =>
       Contains (Table.Current_Namespace, Name)
     and
       Table.Current_Namespace = Table.Current_Namespace'Old
       and
         Table.Find (Name) = Position
     and
       Has_Value (Position)
     and
       Value (Position) = Initial_Value;

   --     procedure Create (Table         : in out Symbol_Table;
   --                       Name          : Symbol_Name;
   --                       Position      : out Cursor)
   --       with
   --         Pre =>
   --           not Contains (Table.Current_Block, Name),
   --       Post =>
   --         Contains (Table.Current_Block, Name)
   --       and
   --         Table.Current_Block = Table.Current_Block'Old
   --         and
   --           Table.Find (Name) = Position
   --       and
   --         not Has_Value (Position);

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

   Stale_Cursor : exception;

private
   --

   -- The structure of the symbol table is as follows: we have a stacks
   -- of Namespaces.  When a new namespace is open, it is pushed on the
   -- stack, when it is closed it is popped out.  The top of the stack
   -- is always the current block.  The stack can never be empty, the
   -- bottom of the stack is the global namespace.
   --
   -- Blocks can have a "parent" that is searched when the symbol is not
   -- found.  The parent list goes from the block to the root.
   --
   -- Every namespace has an ID that is monotonically increased.  Two
   -- namespaces will never have the same ID.
   --
   -- Every namespace has two structure: a vector of Symbol_Value and
   -- a Map mapping symbol_names to vector indexes.  Why this involuted
   -- structure?  Why not just a map sending names to values?  Because
   -- in this way we can have a stable Cursor given by
   -- * The namespace index in the stack
   -- * The namespace ID
   -- * The index of the entry in the value vector
   --
   -- This Cursor is stable against new additions to the table, a feature
   -- that is sometimes required and that maybe is not guaranteed with
   -- the usual library structures.  The ID is not stricly necessary,
   -- but it is useful to check for stale Cursors that refer to past
   -- namespaces.
   --

   type Value_Index is range 1 .. Positive'Last;
   type Namespace_Index is range 1 .. Positive'Last;
   type Namespace_ID is range 0 .. Positive'Last;

   Root_ID        : constant Namespace_ID := Namespace_ID'First;
   Root_Namespace : constant Namespace_Index := Namespace_Index'First;

   package Name_Maps is
     new  Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Symbol_Name,
        Element_Type    => Value_Index,
        Hash            => Hash,
        Equivalent_Keys => Equivalent_Names);

   package Value_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Value_Index,
                                            Element_Type => Symbol_Value);

   package Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Value_Index,
                                            Element_Type => Symbol_Name);

   type Basic_Table;
   type Basic_Table_Access is access Basic_Table;

   type Table_Namespace is
      record
         Table : Basic_Table_Access;
         Index : Namespace_Index;
         ID    : Namespace_ID;
      end record;

   No_Namespace : constant Table_Namespace := (Table => null,
                                           Index => Namespace_Index'Last,
                                           ID    => Namespace_ID'Last);

   type Namespace_Block is
      record
         Name_Map : Name_Maps.Map;
         Values   : Value_Vectors.Vector;
         Names    : Name_Vectors.Vector;
         ID       : Namespace_ID;
         Parent   : Table_Namespace;
      end record;
   --       with Dynamic_Predicate =>
   --         (for all Idx in Namespace_Block.Names.First_Index .. Namespace_Block.Names.Last_Index
   --          => Namespace_Block.Name_Map (Namespace_Block.Names (Idx)) = Idx)
   --       and Names.First_Index = Values.First_Index
   --       and Names.Last_Index = Values.Last_Index;


   type Cursor is
      record
         Namespace : Table_Namespace;
         Idx       : Value_Index;
      end record;

   No_Element : constant Cursor := Cursor'(Namespace => No_Namespace,
                                           Idx       => Value_Index'Last);


   function Image (X : Cursor) return String
   is (if Has_Value (X) then
          "["
       & X.Idx'Image
       & "@"
       & X.Namespace.Index'Image
       & ","
       & X.Namespace.ID'Image
       & "]"
       else
          "NO_ELEMENT");

   package Namespace_Stacks is
     new Ada.Containers.Vectors (Index_Type   => Namespace_Index,
                                 Element_Type => Namespace_Block);

   --     package Value_Holders is
   --       new Ada.Containers.Indefinite_Holders (Symbol_Value);
   --
   --     use type Value_Holders.Holder;
   --
   --     subtype Map_Entry is Value_Holders.Holder;

   subtype Namespace_Stack is Namespace_Stacks.Vector;

   procedure Push (Stack : in out Namespace_Stack;
                   Item  : Namespace_Block);

   procedure Pop (Stack : in out Namespace_Stack);

   type Basic_Table is
      record
         Stack : Namespace_Stacks.Vector;
      end record;

   type Symbol_Table is
     new Finalization.Limited_Controlled
   with
      record
         Counter : Namespace_ID := Root_ID;
         T       : Basic_Table_Access;
      end record;

   overriding procedure Initialize (Object : in out Symbol_Table);

   function Root (T : Symbol_Table) return Table_Namespace
   is (Table_Namespace'(Table => T.T,
                        Index => Root_Namespace,
                        ID    => Root_ID));

   function Current_Namespace (T : Symbol_Table) return Table_Namespace
   is (Table_Namespace'(Table => T.T,
                        Index => T.T.Stack.Last_Index,
                        Id    => T.T.Stack.Last_Element.ID));

   function Parent_Of (Block : Table_Namespace) return Table_Namespace
   is (Block.Table.Stack.Element (Block.Index).Parent);


   function Contains (Block : Table_Namespace;
                      Name  : Symbol_Name)
                      return Boolean
   is (Block.Table.Stack (Block.Index).Name_Map.Contains (Name));

   function Is_Stale (Pos : Cursor) return Boolean
   is (Pos.Namespace /= No_Namespace and then
         (Pos.Namespace.Table.Stack (Pos.Namespace.Index).Id /= Pos.Namespace.Id));

   function Value (Pos : Cursor) return Symbol_Value
   is (if not Is_Stale (Pos) then
         (if Has_Value (Pos) then
               Pos.Namespace.Table.Stack (Pos.Namespace.Index).Values (Pos.Idx)
          else
             raise Uninitialized_Value with String (Name (Pos)))
       else
          raise Stale_Cursor);

   function Has_Value (Pos : Cursor) return Boolean
   is (Pos.Namespace /= No_Namespace);

   function Name (Pos : Cursor) return Symbol_Name
   is (Pos.Namespace.Table.Stack (Pos.Namespace.Index).Names (Pos.Idx));

   function Parent_Of (T     : Symbol_Table;
                       Block : Table_Namespace) return Table_Namespace
   is (Table_Namespace'(Table => Block.Table,
                        Index => Block.Table.Stack (Block.Index).Parent.Index,
                        ID    => Block.Table.Stack (Block.Index).Parent.ID));


end Symbol_Tables.Generic_Symbol_Table;
