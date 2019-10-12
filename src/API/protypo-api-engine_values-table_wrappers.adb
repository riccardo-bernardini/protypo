pragma Ada_2012;
with Ada.Strings.Fixed;

with Protypo.Api.Engine_Values.Array_Wrappers;
with Protypo.Api.Field_Names;

package body Protypo.Api.Engine_Values.Table_Wrappers is
   type Field_Name is
     (
      All_Rows,
      Title,
      All_Titles,
      N_Rows,
      N_Columns
     );
   package My_Fields is
     new Field_Names (Field_Enumerator => Field_Name,
                      Prefix           => "");
   package Row_Wrappers is
     new Array_Wrappers (Element_Type => Engine_Value,
                         Array_Type   => Engine_Value_Array,
                         Create       => Identity);

   --------------------
   -- Default_Titles --
   --------------------

   function Default_Titles (N_Columns : Positive) return Title_Array
   is
      Result : constant Title_Array (1 .. N_Columns) :=
                 (others => Null_Unbounded_String);
   begin
      return Result;
   end Default_Titles;

   --------------------
   -- Default_Labels --
   --------------------

   function Default_Labels (N_Columns : Positive) return Label_Array
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Result : Label_Array (1 .. N_Columns);
   begin
      for I in Result'Range loop
         Result (I) := To_Unbounded_String ("C" & Trim (I'Image, Both));
      end loop;

      return Result;
   end Default_Labels;


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
      case My_Fields.To_Field (Field) is
         when All_Rows =>
         when Title =>
         when All_Titles =>
         when N_Rows =>
         when N_Columns =>
      end case;
   end Get;

   --------------
   -- Is_Field --
   --------------

   function Is_Field (X : Table_Wrapper; Field : Id) return Boolean
   is (My_Fields.Is_Field (Field));

end Protypo.Api.Engine_Values.Table_Wrappers;
