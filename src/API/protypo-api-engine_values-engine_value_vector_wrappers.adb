pragma Ada_2012;
with Ada.Containers;
with Protypo.Api.Engine_Values.Constant_Wrappers;
with Protypo.Api.Engine_Values.Range_Iterators;

with Protypo.Api.Field_Names;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_Io; use Ada.Text_Io;


package body Protypo.Api.Engine_Values.Engine_Value_Vector_Wrappers is
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

   function To_Field (X : Id) return Field_Name
   is (Field_Names_Package.To_Field (X));


   type Array_Iterator is
     new Handlers.Iterator_Interface
   with
      record
         Cursor : Engine_Value_Vectors.Cursor;
         First  : Engine_Value_Vectors.Cursor;
      end record;


   overriding procedure Reset (Iter : in out Array_Iterator);
   overriding procedure Next (Iter : in out Array_Iterator);
   overriding function End_Of_Iteration (Iter : Array_Iterator) return Boolean;
   overriding function Element (Iter : Array_Iterator) return Handler_Value;


   --  function Force_Handler (Item : Engine_Value) return Handler_Value
   --  is (case Item.Class is
   --         when Handler_Classes   =>
   --            Item,
   --
   --         when Int               =>
   --            Constant_Wrappers.To_Handler_Value (Get_Integer (Item)),
   --
   --         when Real              =>
   --            Constant_Wrappers.To_Handler_Value (Get_Float (Item)),
   --
   --         when Text              =>
   --            Constant_Wrappers.To_Handler_Value (Get_String (Item)),
   --
   --         when logical              =>
   --            Constant_Wrappers.To_Handler_Value (Get_Logical (Item)),
   --
   --         when Void | Iterator   =>
   --            raise Constraint_Error);

   overriding procedure Reset (Iter : in out Array_Iterator)
   is
   begin
      Iter.Cursor := Iter.First;
   end Reset;

   overriding procedure Next (Iter : in out Array_Iterator)
   is
   begin
      Engine_Value_Vectors.Next (Iter.Cursor);
   end Next;


   overriding function End_Of_Iteration (Iter : Array_Iterator) return Boolean
   is (not Engine_Value_Vectors.Has_Element (Iter.Cursor));

   overriding function Element (Iter : Array_Iterator) return Handler_Value
   is (Handlers.Force_Handler (Engine_Value_Vectors.Element (Iter.Cursor)));

   ---------
   -- Get --
   ---------

   function Get
     (X : Vector_Handler; Index : Engine_Value_Vectors.Vector) return Handler_Value
   is
      use type Ada.Containers.Count_Type;
   begin
      if Index.Length /= 1 then
         raise Handlers.Out_Of_Range with "Array access with /= 1 index";
      end if;

      if Index.First_Element.Class /= Int then
         raise Handlers.Out_Of_Range with "Array access with non-integer index";
      end if;

      declare
         Idx : constant Positive := Get_Integer (Index.First_Element);
      begin
         if Idx < X.Vect.First_Index or Idx > X.Vect.Last_Index then
            raise Handlers.Out_Of_Range with "Out of bounds index";
         end if;

         return Constant_Wrappers.To_Handler_Value (X.Vect.all (Idx));
      end;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (X : Vector_Handler; Field : Id) return Handler_Value is
      use Constant_Wrappers;
   begin
      case To_Field (Field) is
         when Field_First =>
            return To_Handler_Value (Integer (X.Vect.First_Index));

         when Field_Last =>
            return To_Handler_Value (Integer (X.Vect.Last_Index));

         when Field_Length =>
            return To_Handler_Value (Integer (X.Vect.Length));

         when Field_Iterate =>
            return To_Handler_Value
              (Handlers.Create
                 (Handlers.Iterator_Interface_Access'
                      (new Array_Iterator'(Cursor    => X.Vect.First,
                                           First     => X.Vect.First))));

         when Field_Range =>
            return To_Handler_Value
              (Handlers.Create
                 (Range_Iterators.Create (Start     => Integer (X.Vect.First_Index),
                                          Stop      => Integer (X.Vect.Last_Index))));
      end case;
   end Get;

   --------------
   -- Is_Field --
   --------------

   function Is_Field (X : Vector_Handler; Field : Id) return Boolean
   is (Field_Names_Package.Is_Field (Field));


   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Object : in out Vector_Handler)
   is
   begin
      Object.Vect := new Engine_Value_Vectors.Vector'(Engine_Value_Vectors.Empty_Vector);
   end Initialize;

end Protypo.Api.Engine_Values.Engine_Value_Vector_Wrappers;
