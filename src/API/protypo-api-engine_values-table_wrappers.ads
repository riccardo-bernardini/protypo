with Ada.Containers.Indefinite_Vectors;

package Protypo.Api.Engine_Values.Table_Wrappers is
   type Table_Wrapper (<>) is new Ambivalent_Interface with private;
   type Table_Wrapper_Access is access Table_Wrapper;

   type Name_Array is array (Positive range <>) of Unbounded_String;

   function Make_Table (N_Columns : Positive) return Table_Wrapper_Access
     with Post => Make_Table'Result.N_Columns = N_Columns
       and Make_Table'Result.N_Rows = 0;

   function Make_Table (Column_Names : Name_Array) return Table_Wrapper_Access
     with Post => Make_Table'Result.N_Columns = Column_Names'Length
       and Make_Table'Result.N_Rows = 0;

   function N_Columns (Item : Table_Wrapper) return Positive;

   function N_Rows (Item : Table_Wrapper) return Natural;

   procedure Append (Table : in out Table_Wrapper;
                     Row   :        Engine_Value_Array)
     with
       Pre => Row'Length = Table.N_Columns,
       Post => Table.N_Rows = Table.N_Rows'Old + 1;

   function Get (X     : Table_Wrapper;
                 Index : Engine_Value_Array)
                 return Handler_Value
     with
       Pre => Index'Length = 1
       and then Index (Index'First).Class = Int
     and then Get_Integer(Index(Index'First)) > 0
     and then Get_Integer (Index (Index'First)) <= X.N_Rows,
       Post => Get'Result.Class = Ambivalent_Handler;

   function Get (X     : Table_Wrapper;
                 Field : ID)
                 return Handler_Value;

   function Is_Field (X : Table_Wrapper; Field : Id) return Boolean;

private
   package Engine_Value_Array_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => Engine_Value);

   type Table_Wrapper (N_Columns : Positive)  is new Ambivalent_Interface with
      record
         Names : Name_Array (1 .. N_Columns);
         Rows  : Engine_Value_Array_Vectors.Vector;
      end record;

   function Make_Table (Column_Names : Name_Array) return Table_Wrapper_Access
   is (new Table_Wrapper'(N_Columns => Column_Names'Length,
                          Names     => Column_Names,
                          Rows      => Engine_Value_Array_Vectors.Empty_Vector));

   function Make_Table (N_Columns : Positive) return Table_Wrapper_Access
   is (new Table_Wrapper'(N_Columns => N_Columns,
                          Names     => (others => Null_Unbounded_String),
                          Rows      => Engine_Value_Array_Vectors.Empty_Vector));

   function N_Columns (Item : Table_Wrapper) return Positive
   is (Item.N_Columns);

   function N_Rows (Item : Table_Wrapper) return Natural
   is (Natural (Item.Rows.Length));

end Protypo.Api.Engine_Values.Table_Wrappers;
