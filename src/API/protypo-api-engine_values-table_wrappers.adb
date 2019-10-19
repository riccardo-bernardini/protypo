pragma Ada_2012;
with Ada.Strings.Fixed;

with Protypo.Api.Engine_Values.Array_Wrappers;
with Protypo.Api.Field_Names;
with Protypo.Api.Engine_Values.Constant_Wrappers;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_IO; use Ada.Text_IO;

package body Protypo.Api.Engine_Values.Table_Wrappers is
   type Field_Name is
         (
          Rows,
          Titles,
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

 function Force_Handler (Item : Engine_Value) return Handler_Value
   is (case Item.Class is
          when Handler_Classes =>
             Item,

          when Int             =>
             Constant_Wrappers.To_Handler_Value(Get_Integer(Item)),

          when Real             =>
             Constant_Wrappers.To_Handler_Value (Get_Float (Item)),

          when Text             =>
             Constant_Wrappers.To_Handler_Value (Get_String (Item)),

          when Void | Iterator   =>
             raise Constraint_Error);

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
   -- Default_Titles --
   --------------------

   function Default_Titles (Labels : Label_Array) return Title_Array
   is
      Result : Title_Array (Labels'Range);
   begin
      for k in Labels'Range loop
         Result (K) := Unbounded_String (Labels (K));
      end loop;

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
   -- Create --
   ------------

   function Create (Titles : Title_Array)
                    return Engine_Value_Vectors.Vector_Handler_Access
   is
      Result : constant Engine_Value_Vectors.Vector_Handler_Access :=
                 new Engine_Value_Vectors.Vector_Handler;
   begin
      for K in Titles'Range loop
         Result.Vector.Append (Create (To_String (Titles (K))));
      end loop;

      return Result;
   end Create;

   --------------
   -- Make_Map --
   --------------

   function Make_Map (Labels : Label_Array) return Label_Maps.Map
   is
      Result : Label_Maps.Map;
   begin
      for K in Labels'Range loop
         if Result.Contains (To_Id (Labels (K))) then
            raise Run_Time_Error
                  with "Duplicated column label '" & To_String (Labels (K)) & "'";
         end if;

--           Put_Line ("@@@ inserting [" & To_String (Labels (K)) & "]");

         Result.Insert (Key      => To_Id (Labels (K)),
                        New_Item => K - Labels'First + 1);
      end loop;

      return Result;
   end Make_Map;

   --------------
   -- Make_Row --
   --------------

   function Make_Row (Data   : Engine_Value_Array;
                      Labels : Label_Maps.Map) return Row_Wrapper_Access
   is
      Result : constant Row_Wrapper_Access :=
                 new Row_Wrapper'(Engine_Value_Vectors.Vector_Handler with
                                  Label_To_Column => Labels);
   begin
      for Item of Data loop
         Result.Vector.Append (item);
      end loop;

      return Result;
   end Make_Row;

   ------------
   -- Append --
   ------------

   procedure Append (Table : in out Table_Wrapper; Row : Engine_Value_Array) is
      X : Row_Wrapper_Access;
   begin
      if Row'Length /= Table.N_Columns then
         raise Constraint_Error
               with
                     "Trying to append a row with " &
                     Row'Length'Image & " columns to a table with " &
               Table.N_Rows'Image & " columns";
      end if;

      X := Make_Row (Row, Make_Map (Table.Labels));
      Table.Rows.Vector.Append (Create (Ambivalent_Interface_Access (X)));
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

      return X.Rows.Vector.Ref.all (Get_Integer (Index (Index'First)));
   end Get;

   ---------
   -- Get --
   ---------

   function Get (X : Table_Wrapper; Field : ID) return Handler_Value is


   begin
      case My_Fields.To_Field (Field) is
         when Rows =>
            return Create (Ambivalent_Interface_Access (X.Rows));

         when Titles =>
            return Create (Ambivalent_Interface_Access (X.Titles));

         when N_Rows =>
            return Create (Integer (X.Rows.Vector.Length));

         when N_Columns =>
            return Create (X.N_Columns);

      end case;
   end Get;

   --------------
   -- Is_Field --
   --------------

   function Is_Field (X : Table_Wrapper; Field : Id) return Boolean
   is (My_Fields.Is_Field (Field));

   ---------
   -- Get --
   ---------

   overriding function Get (X     : Row_Wrapper;
                            Field : ID)
                            return Handler_Value
   is
   begin
--        Put_Line ("@@@ proviamo [" & String (field) & "]");
      if not X.Label_To_Column.Contains (Field) then
--           Put_Line ("@@@ manca");
         return Engine_Value_Vectors.Vector_Handler (X).Get (Field);
      else
--           Put_Line ("@@@ trovato");
         return Force_Handler (X.Vector.Element (X.Label_To_Column (Field)));
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   overriding function Get (X     : Row_Wrapper;
                            Index : Engine_Value_Array)
                            return Handler_Value
   is

   begin
      if Index'Length /= 1 then
         raise Run_Time_Error with "too many indexes";
      end if;

      case Index (Index'First).Class is
         when Int =>
            return Engine_Value_Vectors.Vector_Handler (X).Get (Index);

         when Text =>
            declare
               S : constant String := Get_String (Index (Index'First));
            begin
               if Is_Valid_Id (S) and then X.Label_To_Column.Contains (Id (S)) then
                  return X.Vector.Element (X.Label_To_Column (ID (S)));

               else
                  raise Run_Time_Error
                        with "Row ID-indexing with bad/unknown column name"
                        & "'" & S & "'";

               end if;
            end;

         when others =>
            raise Run_Time_Error
                  with "Bad index type: " & Index (Index'First).Class'Image;
      end case;
   end Get;


   --------------
   -- Is_Field --
   --------------

   overriding function Is_Field (X : Row_Wrapper; Field : Id) return Boolean
   is (X.Label_To_Column.Contains (Field) or else
       Engine_Value_Vectors.Vector_Handler (X).Is_Field (Field));

   package body Enumerated_Rows is
      First : constant Integer := Field_Type'Pos (Field_Type'First);

      function Make_Table (Titles : Enumerated_Title_Array) return Table_Wrapper_Access
      is
         T : Title_Array (1 .. N_Fields);
         L : Label_Array (1 .. N_Fields);
      begin
         for Field in Field_Type loop
            T (Field_Type'Pos (Field)-First + T'First) := Titles (Field);
            L (Field_Type'Pos (Field)-First + L'First) := To_Unbounded_String (Field'Image);
         end loop;

         return Make_Table (T, L);
      end Make_Table;

      procedure Append (Table : in out Table_Wrapper;
                        Item  : Aggregate_Type)
      is
         Row : Engine_Value_Array (1 .. N_Fields);
      begin

         for Field in Field_Type loop
            Row (Field_Type'Pos (Field)-First + Row'First) := Item (Field);
         end loop;

         Table.Append (Row);
      end Append;

      ------------------
      -- Append_Array --
      ------------------

      procedure Append_Array (Table : in out Table_Wrapper;
                              Item  : Generic_Aggregate_Array)
      is
      begin
         for Element of Item loop
            Append(Table, Convert(Element));
         end loop;
      end Append_Array;


   --------------------
   -- Default_Titles --
   --------------------

   function Default_Titles return Enumerated_Title_Array
   is
      Result : Enumerated_Title_Array;
   begin

      for field in Field_Type loop
            Result (Field) := To_Unbounded_String (Field'Image);
      end loop;

      return Result;
   end Default_Titles;

   end Enumerated_Rows;


end Protypo.Api.Engine_Values.Table_Wrappers;
