with Symbol_Tables.Generic_Symbol_Table;
with Ada.Text_IO; use Ada.Text_IO;

procedure Symbol_Tables.Test is
   package St is
     new Generic_Symbol_Table (Symbol_Name      => ID_Type,
                               Symbol_Value     => Integer,
                               Hash             => Hash_Id,
                               Equivalent_Names => Equivalent_Id);

   Table : St.Symbol_Table;
begin
   Table.Create (Name          => "pippo",
                 Initial_Value => 12);

   Put_Line (Boolean'Image (Table.Contains ("pippo")));
   Put_Line (St.Value (Table.Find ("pippo"))'Image);

   Table.Open_Internal_Namespace;

   Put_Line (Boolean'Image (Table.Contains ("pippo")));

   Table.Create (Name          => "pippo",
                 Initial_Value => 42);

   Table.Create (Name          => "pluto",
                 Initial_Value => 0);

   Put_Line (Boolean'Image (Table.Contains ("pippo")));
   Put_Line (St.Value (Table.Find ("pippo"))'Image);

   Put_Line (Boolean'Image (Table.Contains ("pluto")));
   Put_Line (St.Value (Table.Find ("pluto"))'Image);

   Table.Open_External_Namespace;

   Put_Line (Boolean'Image (Table.Contains ("pippo")));
   Put_Line (Boolean'Image (Table.Contains ("pluto")));

   Table.Create (Name          => "pippo",
                 Initial_Value => 111);

   Put_Line (Boolean'Image (Table.Contains ("pippo")));
   Put_Line (St.Value (Table.Find ("pippo"))'Image);

   Table.Close_Namespace;

   Put_Line (St.Value (Table.Find ("pippo"))'Image);

   Table.Close_Namespace;

   Put_Line (St.Value (Table.Find ("pippo"))'Image);


end Symbol_Tables.Test;
