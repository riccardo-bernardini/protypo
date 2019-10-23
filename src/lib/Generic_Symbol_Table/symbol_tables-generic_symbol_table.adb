pragma Ada_2012;

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

   procedure Push (Stack : in out Namespace_Stack;
                   Item  : Namespace_Block)
   is
   begin
      Stack.Append (Item);
   end Push;

   procedure Pop (Stack : in out Namespace_Stack)
   is
   begin
      Stack.Delete_Last;
   end Pop;



   function Copy_Globals (T : Symbol_Table) return Symbol_Table
   is
      use Ada.Finalization;

   begin
      return Result : constant Symbol_Table :=
        Symbol_Table'(Limited_Controlled with T => new Basic_Table,
                      Counter                   => <>)
      do
         Result.T.Stack.Append (T.T.Stack.First_Element);
      end return;
   end Copy_Globals;

   -------------
   -- Next_Id --
   -------------

   function Next_Id (Table : in out Symbol_Table) return Namespace_ID
   is
   begin
      Table.Counter := Table.Counter + 1;
      return Table.Counter;
   end Next_Id;

   ----------------
   -- Open_Block --
   ----------------

   procedure Open_Namespace (Table : in out Symbol_Table; Parent : Table_Namespace) is
      New_Block : constant Namespace_Block :=
                    Namespace_Block'(Name_Map => Name_Maps.Empty_Map,
                                     Values   => Value_Vectors.Empty_Vector,
                                     Names    => Name_Vectors.Empty_Vector,
                                     ID       => Next_Id (Table),
                                     Parent   => Parent);

   begin
      Push (Table.T.Stack, New_Block);
   end Open_Namespace;


   -------------------------
   -- Open_Internal_Block --
   -------------------------

   procedure Open_Internal_Namespace (Table : in out Symbol_Table) is
   begin
      Table.Open_Namespace (Table.Current_Namespace);
   end Open_Internal_Namespace;

   -------------------------
   -- Open_External_Block --
   -------------------------

   procedure Open_External_Namespace (Table : in out Symbol_Table) is
   begin
      Table.Open_Namespace (Table.Root);
   end Open_External_Namespace;


   -----------------
   -- Close_Block --
   -----------------

   procedure Close_Namespace (Table : in out Symbol_Table)
   is
      use Ada.Containers;
   begin
--        Debug ("Closing block " & Table.Current.Id'Image);

      if Table.T.Stack.Length = 1 then
         raise Constraint_Error;
      end if;

      Pop (Table.T.Stack);
   end Close_Namespace;

   ----------
   -- Find --
   ----------

   function Find (Table : Symbol_Table; Name : Symbol_Name) return Cursor is
      use type Name_Maps.Cursor;

      Pos           : Name_Maps.Cursor;
      Current_Trial : Namespace_Index := Table.T.Stack.Last_Index;
   begin
      loop
         Debug ("Looking for '" & String (Name) & "' in block " & Current_Trial'Image);

         Pos := Table.T.Stack (Current_Trial).Name_Map.Find (Name);

         if Pos /= Name_Maps.No_Element then
            Debug ("found");
            return Cursor'(Namespace =>
                             Table_Namespace'(Table => Table.t,
                                          Index => Current_Trial,
                                          ID    => Table.T.Stack (Current_Trial).Id),
                           Idx       => Name_Maps.Element(Pos));
         end if;

         exit when Current_Trial = Root_Namespace;

         Current_Trial := Table.T.Stack (Current_Trial).Parent.index;
      end loop;

      Debug ("Not found");
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
      Pos     : constant Namespace_Index := Table.T.Stack.Last_Index;
      Idx     : Value_Index;
   begin
      Debug ("Creating '"
             & String (Name)
             & "' in block ID="
             & Table.T.Stack (Pos).Id'Image);

      Table.T.Stack (Pos).Values.Append (Initial_Value);

      Table.T.Stack (Pos).Names.Append (Name);

      Idx := Table.T.Stack (Pos).Values.Last_Index;

      Table.T.Stack (Pos).Name_Map.Insert (Key      => Name,
                                           New_Item => idx);

      Position := Cursor'(Namespace => Table_Namespace'(Table => Table.T,
                                                    Index => Pos,
                                                    ID    => Table.T.Stack (Pos).Id),
                          Idx       => Idx);
   end Create;

   ------------
   -- Update --
   ------------

   procedure Update (Pos       : Cursor;
                     New_Value : Symbol_Value)
   is

   begin
      Debug ("Updating '"
             & String (Name (Pos))
             & "' in block "
             & Pos.Namespace.ID'Image
             & " to [" & Image (New_Value) & "]");

      Pos
        .Namespace.Table.Stack (Pos.Namespace.Index)
        .Values.Replace_Element (Pos.Idx, New_Value);
   end Update;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Object : in out Symbol_Table) is
   begin

      Object.Counter := Root_ID;
      Object.T := new Basic_Table'(Stack => Namespace_Stacks.Empty_Vector);

      Push (Stack => Object.T.Stack,
            Item  => Namespace_Block'(Name_Map => Name_Maps.Empty_Map,
                                      Values   => Value_Vectors.Empty_Vector,
                                      Names    => Name_Vectors.Empty_Vector,
                                      ID       => Root_ID,
                                      Parent   => No_Namespace));
   end Initialize;

end Symbol_Tables.Generic_Symbol_Table;
