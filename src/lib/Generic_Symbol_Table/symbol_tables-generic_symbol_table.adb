pragma Ada_2012;

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body Symbol_Tables.Generic_Symbol_Table is
   Activate_Debug : constant Boolean := False;

   Printer : Value_Printer := null;

   procedure Set_Printer (Callback : Value_Printer) is
   begin
      Printer := Callback;
   end Set_Printer;

   function Image (X : Symbol_Value) return String
   is (if Printer = null then
          "???"
       else
          Printer (X));


   procedure Debug (X : String) is
   begin
      if Activate_Debug then
         Put_Line (Standard_Error, X);
      end if;
   end Debug;

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
                                                           Old_Current => Table.Current,
                                                           ID          => Table.Counter + 1);
   begin
      Debug ("Opening block " & New_Block.Id'Image & " child of " & Parent.Id'Image);
      Table.Current := New_Block;
      Table.Counter := New_Block.Id;
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
      Debug ("Closing block " & Table.Current.Id'Image);

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
         Debug ("Looking for '" & String (Name) & "' in block " & Current_Trial.ID'Image);

         Pos := Current_Trial.Map.Find (Name);

         if Pos /= Symbol_Maps.No_Element then
            Debug("found");
            return Cursor'(Valid => True,
                           Block => Current_Trial,
                           Key   => To_Unbounded_String (String (name)));
         end if;

         Current_Trial := Current_Trial.Parent;
      end loop;

      Debug("Not found");
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
      Table.Create (Name, Position);
      Update (Position, Initial_Value);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create (Table         : in out Symbol_Table;
                     Name          : Symbol_Name;
                     Position      : out Cursor)
   is
      Ignored : Boolean;
   begin
      Debug ("Creating '" & String (Name) & "' in block " & Table.Current.ID'Image);

      Table.Current.Map.Insert (Key      => Name,
                                New_Item => Value_Holders.Empty_Holder);

      Position := Cursor'(Valid => True,
                          Block => Table.Current,
                          Key   => To_Unbounded_String (String (Name)));
   end Create;

   ------------
   -- Update --
   ------------

   procedure Update (Pos       : Cursor;
                     New_Value : Symbol_Value)
   is
      use type Symbol_Maps.Cursor;

      C : constant Symbol_Maps.Cursor :=
            Pos.Block.Map.Find (Symbol_Name (To_String (Pos.Key)));
   begin
      if C = Symbol_Maps.No_Element then
         raise Constraint_Error with "Stale symbol table cursor";
      end if;

      Debug ("Updating '"
             & String (Name (Pos))
             & "' in block "
             & Pos.Block.ID'Image
             & " to [" & Image (New_Value) & "]");

      Pos.Block.Map.Replace_Element (C, Value_Holders.To_Holder (New_Value));
   end Update;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Object : in out Symbol_Table) is
   begin
      Object.Root := new Basic_Block'(Map         => <>,
                                      Parent      => No_Block,
                                      Old_Current => No_Block,
                                      ID          => Block_ID'First);

      Object.Current := Object.Root;
      Object.Counter := Object.Root.ID;
   end Initialize;

end Symbol_Tables.Generic_Symbol_Table;
