pragma Ada_2012;

with Ada.Unchecked_Deallocation;

package body Symbol_Tables.Generic_Symbol_Table is

   function Copy_Globals (T : Symbol_Table) return Symbol_Table
   is
   begin
      return Result : Symbol_Table do
         Result.Root.Map := T.Root.Map;
      end return;
   end Copy_Globals;

   ----------------
   -- Open_Block --
   ----------------

   procedure Open_Block (Table : in out Symbol_Table; Parent : Table_Block) is
      New_Block : constant Table_Block := new Basic_Block'(Map         => <>,
                                                           Parent      => Parent,
                                                           Old_Current => Table.Current);
   begin
      Table.Current := New_Block;
   end Open_Block;


   -------------------------
   -- Open_Internal_Block --
   -------------------------

   procedure Open_Internal_Block (Table : in out Symbol_Table) is
   begin
      Table.Open_Block (Table.Current_Block);
   end Open_Internal_Block;

   -------------------------
   -- Open_External_Block --
   -------------------------

   procedure Open_External_Block (Table : in out Symbol_Table) is
   begin
      Table.Open_Block (Table.Root);
   end Open_External_Block;


   -----------------
   -- Close_Block --
   -----------------

   procedure Close_Block (Table : in out Symbol_Table)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Basic_Block,
                                        Name   => Table_Block);
      Old_Current : Table_Block;
   begin
      if Table.Current = Table.Root then
         raise Constraint_Error;
      end if;

      Old_Current := Table.Current.Old_Current;
      Free (Table.Current);

      Table.Current := Old_Current;
   end Close_Block;

   ----------
   -- Find --
   ----------

   function Find (Table : Symbol_Table; Name : Symbol_Name) return Cursor is
      use type Symbol_Maps.Cursor;

      Pos           : Symbol_Maps.Cursor;
      Current_Trial : Table_Block := Table.Current_Block;
   begin
      pragma Assert (Current_Trial /= No_Block);

      while Current_Trial /= No_Block loop
         Pos := Current_Trial.Map.Find (Name);

         if Pos /= Symbol_Maps.No_Element then
            return Cursor'(Block           => Current_Trial,
                           Internal_Cursor => Pos);
         end if;

         Current_Trial := Current_Trial.Parent;
      end loop;

      return No_Element;
   end Find;

   ------------
   -- Create --
   ------------

   procedure Create
     (Table         : in out Symbol_Table;
      Name          : Symbol_Name;
      Initial_Value : Symbol_Value)
   is
      Ignored : Cursor;
   begin
      Table.Create (Name          => Name,
                    Initial_Value => Initial_Value,
                    Position      => Ignored);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Table         : in out Symbol_Table;
      Name          : Symbol_Name;
      Initial_Value : Symbol_Value;
      Position      : out Cursor)
   is
      Ignored : Boolean;
   begin
      Position.Block := Table.Current;

      Table.Current.Map.Insert (Key      => Name,
                                New_Item => Initial_Value,
                                Position => Position.Internal_Cursor,
                                Inserted => Ignored);
   end Create;

   ------------
   -- Update --
   ------------

   procedure Update (Pos       : Cursor;
                     New_Value : Symbol_Value) is
   begin
      Pos.Block.Map.Replace_Element (Pos.Internal_Cursor, New_Value);
   end Update;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Object : in out Symbol_Table) is
   begin
      Object.Root := new Basic_Block'(Map         => <>,
                                      Parent      => No_Block,
                                      Old_Current => No_Block);

      Object.Current := Object.Root;
   end Initialize;

end Symbol_Tables.Generic_Symbol_Table;
