pragma Ada_2012;
with Protypo.Api.Engine_Values.Array_Wrappers;

package body Protypo.Api.Engine_Values.Table_Wrappers is
   package Row_Wrappers is
     new Array_Wrappers (Element_Type => Engine_Value,
                         Array_Type   => Engine_Value_Array,
                         Create       => identity);

   ------------
   -- Append --
   ------------

   procedure Append (Table : in out Table_Wrapper; Row : Engine_Value_Array) is
   begin
      if Row'Length /= Table.N_Columns then
         raise Constraint_Error
           with
             "Trying to append a row with " &
             Row'Length'Image & " columns to a table with " &
           Table.N_Rows'Image & " columns";
      end if;

      Table.Rows.Append (Row_Wrappers.Create (Row));
   end Append;

   ---------
   -- Get --
   ---------

   function Get
     (X : Table_Wrapper; Index : Engine_Value_Array) return Handler_Value
   is
   begin
      if not (Index'Length = 1
              and then Index (Index'First).Class = Int
              and then Get_Integer (Index (Index'First)) > 0
              and then Get_Integer (Index (Index'First)) <= X.N_Rows)
      then
         raise Constraint_Error with "Bad index for table";
      end if;

      return X.Rows (Get_Integer (Index (Index'First)));
   end Get;

   ---------
   -- Get --
   ---------

   function Get (X : Table_Wrapper; Field : ID) return Handler_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
      return raise Program_Error with "Unimplemented function Get";
   end Get;

   --------------
   -- Is_Field --
   --------------

   function Is_Field (X : Table_Wrapper; Field : Id) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Is_Field unimplemented");
      return raise Program_Error with "Unimplemented function Is_Field";
   end Is_Field;

end Protypo.Api.Engine_Values.Table_Wrappers;
