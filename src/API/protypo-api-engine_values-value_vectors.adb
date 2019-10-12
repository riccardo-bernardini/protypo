pragma Ada_2012;
with Protypo.Api.Engine_Values.Constant_Wrappers;

with Protypo.Api.Engine_Values.Range_Iterators;

with Protypo.Api.Field_Names;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_IO; use Ada.Text_IO;


package body Protypo.Api.Engine_Values.Value_Vectors is
   type Field_Name is
     (
      Field_First,
      Field_Last,
      Field_Range,
      Field_Iterate,
      Field_Length
     );

   package Field_Names_Package is
     new Field_Names (Field_Enumerator => Field_Name,
                      Prefix           => "Field_");

   function To_Field (X : String) return Field_Name
   is (Field_Names_Package.To_Field (X));


   type Array_Iterator is
     new Iterator_Interface
   with
      record
         Cursor    : Index_Type;
         Container : Vector_Access;
      end record;


   overriding procedure Reset (Iter : in out Array_Iterator);
   overriding procedure Next (Iter : in out Array_Iterator);
   overriding function End_Of_Iteration (Iter : Array_Iterator) return Boolean;
   overriding function Element (Iter : Array_Iterator) return Handler_Value;

   overriding procedure Reset (Iter : in out Array_Iterator)
   is
   begin
      Iter.Cursor := Iter.Container.First_Index;
   end Reset;

   overriding procedure Next (Iter : in out Array_Iterator)
   is
   begin
      if not Iter.End_Of_Iteration then
         Iter.Cursor := Iter.Cursor + 1;
      end if;
   end Next;

   function Force_Handler (Item : Engine_Value) return Handler_Value
   is (case Item.Class is
          when Handler_Classes   =>
             Item,

          when Int               =>
             Constant_Wrappers.To_Handler_Value (Get_Integer (Item)),

          when Real              =>
             Constant_Wrappers.To_Handler_Value (Get_Float (Item)),

          when Text              =>
             Constant_Wrappers.To_Handler_Value (Get_String (Item)),

          when Void | Iterator   =>
             raise Constraint_Error);


   overriding function End_Of_Iteration (Iter : Array_Iterator) return Boolean
   is (Iter.Cursor > Iter.Container.Last_Index);

   overriding function Element (Iter : Array_Iterator) return Handler_Value
   is (Force_Handler (Iter.Container.all (Iter.Cursor)));

   ---------
   -- Get --
   ---------

   function Get
     (X : Vector_Handler; Index : Engine_Value_Array) return Handler_Value
   is
   begin
      if Index'Length /= 1 then
         raise Out_Of_Range with "Array access with /= 1 index";
      end if;

      if Index (Index'First).Class /= Int then
         raise Out_Of_Range with "Array access with non-integer index";
      end if;

      declare
         Idx : constant Index_Type := Index_Type (Get_Integer (Index (Index'First)));
      begin
         if Idx < X.Vect.First_Index or Idx > X.Vect.Last_Index then
            raise Out_Of_Range with "Out of bounds index";
         end if;

         return Constant_Wrappers.To_Handler_Value (X.Vect.all (Idx));
      end;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (X : Vector_Handler; Field : ID) return Handler_Value is
      use Constant_Wrappers;
   begin
      case To_Field (Field) is
         when Field_First =>
            return To_Handler_Value (Integer(X.Vect.First_Index));

         when Field_Last =>
            return To_Handler_Value (Integer(X.Vect.Last_Index));

         when Field_Length =>
            return To_Handler_Value (Integer (X.Vect.Length));

         when Field_Iterate =>
            return To_Handler_Value
              (Create
                 (Iterator_Interface_Access'
                      (new Array_Iterator'(Cursor    => X.Vect.First_Index,
                                           Container => X.Vect))));

         when Field_Range =>
            return To_Handler_Value
              (Create
                 (Range_Iterators.Create (Start     => Integer(X.Vect.First_Index),
                                          Stop      => Integer(X.Vect.Last_Index))));
      end case;
   end Get;

   --------------
   -- Is_Field --
   --------------

   function Is_Field (X : Vector_Handler; Field : Id) return Boolean
   is (Field_Names_Package.Is_Field (Field));

end Protypo.Api.Engine_Values.Value_Vectors;
