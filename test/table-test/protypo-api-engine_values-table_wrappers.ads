with Protypo.Api.Engine_Values.Handlers;
with Protypo.Api.Engine_Values.Engine_Value_Holders;
with Protypo.Api.Engine_Values.Engine_Value_Vectors;
with Protypo.Api.Engine_Values.Array_Wrappers;

with Ada.Containers.Indefinite_Ordered_Maps;

package Protypo.Api.Engine_Values.Table_Wrappers is
   use type Ada.Containers.Count_Type;

   type Table_Wrapper (<>) is new Handlers.Ambivalent_Interface with private;
   type Table_Wrapper_Access is access Table_Wrapper;

   type Title_Array is array (Positive range <>) of Unbounded_String;
   type Label_Array is array (Positive range <>) of Unbounded_ID;

   function Make_Table (N_Columns : Positive) return Table_Wrapper_Access
     with Post => Make_Table'Result.N_Columns = N_Columns
     and Make_Table'Result.N_Rows = 0;

   function Make_Table (Column_Names : Title_Array) return Table_Wrapper_Access
     with Post => Make_Table'Result.N_Columns = Column_Names'Length
     and Make_Table'Result.N_Rows = 0;

   function Make_Table (Labels : Label_Array) return Table_Wrapper_Access
     with Post => Make_Table'Result.N_Columns = Labels'Length
     and Make_Table'Result.N_Rows = 0;

   function Make_Table (Column_Names : Title_Array;
                        Labels       : Label_Array) return Table_Wrapper_Access
     with
       Pre => Column_Names'Length = Labels'Length,
       Post => Make_Table'Result.N_Columns = Column_Names'Length
       and Make_Table'Result.N_Rows = 0;

   function N_Columns (Item : Table_Wrapper) return Positive;

   function N_Rows (Item : Table_Wrapper) return Natural;

   procedure Append (Table : in out Table_Wrapper;
                     Row   :        Engine_Value_Vectors.Vector)
     with
       Pre => Natural(Row.Length) = Table.N_Columns,
       Post => Table.N_Rows = Table.N_Rows'Old + 1;

   function Get (X     : Table_Wrapper;
                 Index : Engine_Value_Vectors.Vector)
                 return Handler_Value
     with
       Pre => Index.Length = 1
       and then Index.First_Element.Class = Int
     and then Get_Integer (Index.First_Element) > 0
     and then Get_Integer (Index.First_Element) <= X.N_Rows,
     Post => Get'Result.Class = Ambivalent_Handler;

   function Get (X     : Table_Wrapper;
                 Field : ID)
                 return Handler_Value;

   function Is_Field (X : Table_Wrapper; Field : Id) return Boolean;

   generic
      type Field_Type is (<>);
   package Enumerated_Rows is
      type Aggregate_Type is array (Field_Type) of Engine_Value_Holders.Holder;

      type Enumerated_Title_Array is array (Field_Type) of Unbounded_String;

      N_Fields : constant Integer :=
                   Field_Type'Pos (Field_Type'Last)-Field_Type'Pos (Field_Type'First)+1;

      function Make_Table return Table_Wrapper_Access
            with Post => Make_Table'Result.N_Columns = N_Fields;

      function Make_Table (Titles : Enumerated_Title_Array) return Table_Wrapper_Access
            with Post => Make_Table'Result.N_Columns = N_Fields;

      procedure Append (Table : in out Table_Wrapper;
                        Item  : Aggregate_Type);

      generic
         type Ada_Aggregate is limited private;
         type Aggregate_Index is (<>);

         type Generic_Aggregate_Array is
               array (Aggregate_Index range <>) of Ada_Aggregate;

         with function Convert (X : Ada_Aggregate) return Aggregate_Type;
      procedure Append_Array (Table : in out Table_Wrapper;
                              Item  : Generic_Aggregate_Array);

   private
      function Default_Titles return Enumerated_Title_Array;

      function Make_Table return Table_Wrapper_Access
      is (Make_Table (Default_Titles));

   end Enumerated_Rows;
private
   package Label_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => ID,
                                                 Element_Type => Positive);

   type Row_Wrapper is
     new Array_Wrappers.Array_Wrapper
   with
      record
         Label_To_Column : Label_Maps.Map;
      end record;


   type Row_Wrapper_Access is access Row_Wrapper;

   function Make_Row (Data   : Engine_Value_Vectors.Vector;
                      Labels : Label_Maps.Map) return Row_Wrapper_Access;

   function Make_Map (Labels : Label_Array) return Label_Maps.Map
     with
       Pre => (for all I in Labels'Range =>
                 (for all J in I + 1 .. Labels'Last =>
                        Labels (I) /= Labels (J))),
     Post => Integer (Make_Map'Result.Length) = Labels'Length;

   overriding function Get (X     : Row_Wrapper;
                            Field : ID)
                            return Handler_Value;

   overriding function Get (X     : Row_Wrapper;
                            Index : Engine_Value_Vectors.Vector)
                            return Handler_Value;

   overriding function Is_Field (X : Row_Wrapper; Field : Id) return Boolean;


   type Table_Wrapper (N_Columns : Positive) is
     new Ambivalent_Interface
   with
      record
         Titles : Engine_Value_Vectors.Vector_Handler_Access;
         Labels : Label_Array (1 .. N_Columns);
         Rows   : Engine_Value_Vectors.Vector_Handler_Access;
      end record;

   function Default_Titles (N_Columns : Positive) return Title_Array
     with Post => Default_Titles'Result'Length = N_Columns;

   function Default_Titles (Labels : Label_Array) return Title_Array
     with Post => Default_Titles'Result'Length = Labels'Length;

   function Default_Labels (N_Columns : Positive) return Label_Array
     with Post => Default_Labels'Result'Length = N_Columns;

   function Create (Titles : Title_Array)
                    return Engine_Value_Vectors.Vector_Handler_Access;

--     function Make_Table (Column_Names : Title_Array;
--                          Labels       : Label_Array) return Table_Wrapper_Access
--     is (new Table_Wrapper'(Titles => Create (Column_Names),
--                            Rows   => new Row_Wrapper'(Engine_Value_Vectors.Vector_Handler with
--                                                         Label_To_Column => Make_Map (Labels))));
   function Make_Table (Column_Names : Title_Array;
                        Labels       : Label_Array) return Table_Wrapper_Access
   is (new Table_Wrapper'(N_Columns => Column_Names'Length,
                          Titles    => Create (Column_Names),
                          Labels    => Labels,
                          Rows      => new Engine_Value_Vectors.Vector_Handler));

   function Make_Table (N_Columns : Positive) return Table_Wrapper_Access
   is (Make_Table (Default_Titles (N_Columns), Default_Labels (N_Columns)));

   function Make_Table (Column_Names : Title_Array) return Table_Wrapper_Access
   is (Make_Table (Column_Names, Default_Labels (Column_Names'Length)));

   function Make_Table (Labels : Label_Array) return Table_Wrapper_Access
   is (Make_Table (Default_Titles (Labels), Labels));


   function N_Columns (Item : Table_Wrapper) return Positive
   is (Integer (Item.Titles.Vector.Length));

   function N_Rows (Item : Table_Wrapper) return Natural
   is (Natural (Item.Rows.Vector.Length));

end Protypo.Api.Engine_Values.Table_Wrappers;
