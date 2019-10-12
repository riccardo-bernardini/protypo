--  with Ada.Containers.Indefinite_Vectors;

with Protypo.Api.Engine_Values.Value_Vectors;

package Protypo.Api.Engine_Values.Table_Wrappers is
   type Table_Wrapper (<>) is new Ambivalent_Interface with private;
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
     and then Get_Integer (Index (Index'First)) > 0
     and then Get_Integer (Index (Index'First)) <= X.N_Rows,
     Post => Get'Result.Class = Ambivalent_Handler;

   function Get (X     : Table_Wrapper;
                 Field : ID)
                 return Handler_Value;

   function Is_Field (X : Table_Wrapper; Field : Id) return Boolean;

private
   package Engine_Value_Vectors is
     new Value_Vectors (Index_Type => Positive);

   type Row_Wrapper (N_Columns : Positive) is
     new Engine_Value_Vectors.Vector_Handler
   with
      record
         Labels : Label_Array (1 .. N_Columns);

      end record;


   type Table_Wrapper (N_Columns : Positive)  is new Ambivalent_Interface with
      record
         Names  : Title_Array (1 .. N_Columns);
         Rows   : Row_Wrapper (N_Columns);
      end record;

   function Default_Titles (N_Columns : Positive) return Title_Array
     with Post => Default_Titles'Result'Length = N_Columns;

   function Default_Labels (N_Columns : Positive) return Label_Array
     with Post => Default_Labels'Result'Length = N_Columns;


   function Make_Table (Column_Names : Title_Array;
                        Labels       : Label_Array) return Table_Wrapper_Access
   is (new Table_Wrapper'(N_Columns => Column_Names'Length,
                          Names     => Column_Names,
--                            Labels    => Labels,
                          Rows      => (Engine_Value_Vectors.Vector_Handler with
                                          N_Columns => Column_Names'Length,
                                        Labels    => Labels)));

   function Make_Table (N_Columns : Positive) return Table_Wrapper_Access
   is (Make_Table (Default_Titles (N_Columns), Default_Labels (N_Columns)));

   function Make_Table (Column_Names : Title_Array) return Table_Wrapper_Access
   is (Make_Table (Column_Names, Default_Labels (Column_Names'Length)));

   function Make_Table (Labels : Label_Array) return Table_Wrapper_Access
   is (Make_Table (Default_Titles (Labels'Length), Labels));


   function N_Columns (Item : Table_Wrapper) return Positive
   is (Item.N_Columns);

   function N_Rows (Item : Table_Wrapper) return Natural
   is (Natural (Item.Rows.Vector.length));

end Protypo.Api.Engine_Values.Table_Wrappers;
